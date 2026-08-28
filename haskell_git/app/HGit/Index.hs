{-# LANGUAGE ScopedTypeVariables #-}

module HGit.Index (
  readIndex,
  writeIndex,
  isEntryModified,
  getEntryHash,
  getStatData,
  getEntryStatus,
  fileToEntry,
  makeEntryAndStat,
  findEntryByPath,
  makeBlankEntry,
  Index (..),
  IndexEntry (..),
  IndexEntries,
  EntryStatus (..),
  FileMode (..),
  treeIndexFoldlM,
  TreeIndexFold (..),
  treeIndexDiffFoldlM,
  TreeIndexDiff (..),
) where

import Control.Monad.ST (runST)
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Attoparsec.Binary as AB
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isUpperCase)
import qualified Data.String
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VS
import Data.Word (Word32)
import Debug.Trace (trace)
import HGit.Object (Hash, ObjType (BlobObj), Object (objHash), getFileHash, hashLazy, makeObject)
import HGit.Repository (Repository, WithRepository, WorkTreePath, gitPath, worktreePath)
import HGit.Tree (FileMode (..), Tree (treeItems), TreeItem (..), readTree)
import HGit.Types (Hash (hashBS), byteHashBuilder, byteHashParser)
import HGit.Utils (insertManySorted, nameParser, runParserUnsafe, throwErr, throwStrErr)
import Relude
import System.Directory (executable)
import System.FilePath.Posix ((</>))
import qualified System.Posix.Files as Files
import qualified UnliftIO.Directory as Dir

data FileStat = FileStat
  { statDataModified :: (Word32, Word32)
  , statMDataModified :: (Word32, Word32)
  , statDev :: Word32
  , statIno :: Word32
  , statUid :: Word32
  , statGid :: Word32
  , statSize :: Word32
  }
  deriving (Show, Eq)

data IndexExtension = IndexExtension {extSig :: BS.ByteString, extRaw :: BS.ByteString} deriving (Show)

data IndexEntry = IndexEntry
  { ieStat :: FileStat
  , ieMode :: FileMode
  , ieObjHash :: Hash
  , ieFlags :: Word16
  , ieExtFlags :: Maybe Word16
  , iePath :: WorkTreePath
  }
  deriving (Show)

instance Eq IndexEntry where
  (==) :: IndexEntry -> IndexEntry -> Bool
  a == b = iePath a == iePath b -- TODO: stage flag sorting if equal

instance Ord IndexEntry where
  compare :: IndexEntry -> IndexEntry -> Ordering
  compare a b = compare (iePath a) (iePath b) -- TODO: stage flag sorting if equal

type IndexEntries = V.Vector IndexEntry

data Index = Index
  { idxVersion :: Word32
  , idxEntries :: IndexEntries -- sorted by path
  , idxExtensions :: [IndexExtension]
  }
  deriving (Show)

extensionParser :: A.Parser IndexExtension
extensionParser = do
  extSig <- A.take 4
  size <- AB.anyWord32be
  extRaw <- A.take (fromIntegral size)
  return IndexExtension{..}

extensionBuilder :: IndexExtension -> B.Builder
extensionBuilder IndexExtension{..} = W.execWriter $ do
  W.tell $ B.byteString extSig
  W.tell $ B.word32BE $ fromIntegral $ BS.length extRaw
  W.tell $ B.byteString extRaw

-- https://github.com/git/git/blob/master/Documentation/gitformat-index.adoc#index-entry
indexEntryParser :: A.Parser IndexEntry
indexEntryParser = nameParser "indexEntryParser" $ do
  statDataModified <- ((,) <$> AB.anyWord32be <*> AB.anyWord32be) <?> "mod"
  statMDataModified <- ((,) <$> AB.anyWord32be <*> AB.anyWord32be) <?> "mmod"
  statDev <- AB.anyWord32be <?> "dev"
  statIno <- AB.anyWord32be <?> "ino"

  _ <- AB.word16be 0 <?> "null"
  packed <- AB.anyWord16be
  let mode = packed `Bits.shiftR` 12
  let perms = packed .&. 0x1FF
  let ieMode = case (mode, perms) of
        (0b1000, 0o644) -> RegularFile
        (0b1000, 0o755) -> ExecutableFile
        (0b1010, _) -> Symlink
        (0b1110, _) -> Gitlink
        _ -> throwErr "indexEntryParser" "unknown mode"

  statUid <- AB.anyWord32be
  statGid <- AB.anyWord32be
  statSize <- AB.anyWord32be
  ieObjHash <- byteHashParser
  ieFlags <- AB.anyWord16be
  let ieExtFlags = Nothing
  -- TODO: v3+flags <- AB.anyWord16be -- if they're there, add to headerlen

  let headerLen = 62
  iePath <- BSC8.unpack <$> A8.takeTill (== '\0')
  _ <- A8.char '\0'
  let bytesRead = headerLen + length iePath + 1

  -- TODO: if v < 4
  let bytesToAlignment = (8 - bytesRead) `mod` 8
  _ <- A8.count bytesToAlignment (A8.char '\0')

  let ieStat = FileStat{..}
  return (IndexEntry{..})

entryBuilder :: IndexEntry -> B.Builder
entryBuilder IndexEntry{..} = W.execWriter $ do
  let FileStat{..} = ieStat
  W.tell $ modifiedBuilder statDataModified
  W.tell $ modifiedBuilder statMDataModified
  W.tell $ B.word32BE statDev
  W.tell $ B.word32BE statIno

  let (mode, perms) = case ieMode of
        RegularFile -> (0b1000, 0o644)
        ExecutableFile -> (0b1000, 0o755)
        Symlink -> (0b1010, 0)
        Gitlink -> (0b1110, 0)
        Directory -> throwErr "entryBuilder" "unexpected directory entry"
  let packed = perms .|. (mode `Bits.shiftL` 12)
  W.tell $ B.word16BE 0
  W.tell $ B.word16BE packed

  W.tell $ B.word32BE statUid
  W.tell $ B.word32BE statGid
  W.tell $ B.word32BE statSize
  W.tell $ byteHashBuilder ieObjHash
  W.tell $ B.word16BE ieFlags
  case ieExtFlags of
    Just extFlags -> W.tell $ B.word16BE extFlags
    Nothing -> pass

  W.tell $ B.byteString $ BSC8.pack iePath
  W.tell $ B.word8 0

  let headerLen = if isJust ieExtFlags then 64 else 62
      bytesWritten = headerLen + length iePath + 1
      padLen = (8 - bytesWritten) `mod` 8

  -- TODO: if v < 4
  W.tell $ mconcat $ replicate padLen (B.word8 0)
 where
  modifiedBuilder (a, b) = B.word32BE a <> B.word32BE b

indexParser :: A.Parser Index
indexParser = nameParser "indexParser" $ do
  _ <- A.string "DIRC"
  idxVersion <- AB.anyWord32be
  when (idxVersion /= 2) $ throwErr "indexParser" "unsupported index version"
  len <- AB.anyWord32be
  idxEntries <- V.generateM (fromIntegral len) (const indexEntryParser)
  extensions <- many extensionParser

  -- filter out optional
  let idxExtensions = filter (not . isUpperCase . BSC8.head . extSig) extensions

  unless (null idxExtensions) $
    throwStrErr "indexParser" $
      "unsupported extensions: " ++ show (BSC8.unpack . extSig <$> idxExtensions)

  _hash <- byteHashParser
  _ <- A.endOfInput
  return Index{..}

indexBuilder :: Index -> BSL.LazyByteString
indexBuilder Index{..} = do
  let body = W.execWriter $ do
        W.tell $ B.byteString "DIRC"
        W.tell $ B.word32BE idxVersion
        let count = length idxEntries
        W.tell $ B.word32BE (fromIntegral count)
        W.tell $ foldMap entryBuilder idxEntries
        W.tell $ foldMap extensionBuilder idxExtensions
      bodyBSL = B.toLazyByteString body
      checksum = fromShort $ hashBS $ hashLazy bodyBSL
  bodyBSL <> fromStrict checksum

writeIndex :: Index -> WithRepository ()
writeIndex index = do
  indexPath <- gitPath ["index"]
  let raw = indexBuilder $! index
  writeFileLBS indexPath raw

-- https://github.com/git/git/blob/master/Documentation/gitformat-index.adoc
readIndex :: WithRepository Index
readIndex = do
  indexPath <- gitPath ["index"]
  raw <- readFileLBS indexPath
  return $ runParserUnsafe indexParser raw

getStatData :: (MonadIO m) => FilePath -> m FileStat
getStatData path = liftIO $ do
  stat <- Files.getSymbolicLinkStatus path
  let (ctimeSec, ctimeNsec) = extractTimeParts (Files.statusChangeTimeHiRes stat)
      (mtimeSec, mtimeNsec) = extractTimeParts (Files.modificationTimeHiRes stat)
  pure
    FileStat
      { statDataModified = (ctimeSec, ctimeNsec)
      , statMDataModified = (mtimeSec, mtimeNsec)
      , statDev = fromIntegral (Files.deviceID stat)
      , statIno = fromIntegral (Files.fileID stat)
      , statUid = fromIntegral (Files.fileOwner stat)
      , statGid = fromIntegral (Files.fileGroup stat)
      , statSize = fromIntegral (Files.fileSize stat)
      }
 where
  extractTimeParts psx =
    let psxReal = toRational psx
        sec = floor psxReal :: Integer
        nsec = floor ((psxReal - fromInteger sec) * 1000000000) :: Integer
     in (fromIntegral sec, fromIntegral nsec)

fileToEntry :: FilePath -> WithRepository IndexEntry
fileToEntry iePath = do
  realPath <- worktreePath [iePath]
  ieObjHash <- getFileHash realPath

  symlink <- Dir.pathIsSymbolicLink realPath
  -- TODO: gitlink
  ieMode <-
    if symlink
      then return Symlink
      else do
        isExec <- executable <$> Dir.getPermissions realPath
        return $ if isExec then ExecutableFile else RegularFile

  makeEntryAndStat iePath realPath ieObjHash ieMode

blankFileStat :: FileStat
blankFileStat =
  FileStat
    { statDataModified = (0, 0)
    , statMDataModified = (0, 0)
    , statDev = 0
    , statIno = 0
    , statUid = 0
    , statGid = 0
    , statSize = 0
    }

makeBlankEntry :: WorkTreePath -> Hash -> FileMode -> IndexEntry
makeBlankEntry iePath ieObjHash ieMode = do
  let ieStat = blankFileStat
  let ieFlags = fromIntegral $ length iePath
  let ieExtFlags = Nothing
  IndexEntry{..}

makeEntryAndStat :: (MonadIO m) => WorkTreePath -> FilePath -> Hash -> FileMode -> m IndexEntry
makeEntryAndStat iePath realPath hash ieMode = do
  let ieObjHash = hash
  ieStat <- getStatData realPath

  let ieFlags = fromIntegral $ length iePath
  let ieExtFlags = Nothing

  return IndexEntry{..}

{- | checks if the file from the entry has changed
for git ls-files -m
-}
isEntryModified :: IndexEntry -> WithRepository Bool
isEntryModified entry = do
  newHash <- getEntryHash entry
  case newHash of
    Just hash -> return $ hash /= ieObjHash entry
    Nothing -> return True

getEntryHash :: IndexEntry -> WithRepository (Maybe Hash)
getEntryHash entry = do
  -- TODO:  if hash equal, update stat data
  filePath <- worktreePath [iePath entry]
  fileExists <- Dir.doesFileExist filePath
  if fileExists
    then do
      stat <- getStatData filePath
      if ieStat entry == stat
        then return $ Just $ ieObjHash entry
        else do Just <$> getFileHash filePath
    else return Nothing

data EntryStatus = EntryDeleted | EntryModified deriving (Show)

getEntryStatus :: IndexEntry -> WithRepository (Maybe (EntryStatus, IndexEntry))
getEntryStatus entry = do
  newHash <- getEntryHash entry
  case newHash of
    Just hash ->
      if hash == ieObjHash entry
        then return Nothing
        else return $ Just (EntryModified, entry)
    Nothing -> return $ Just (EntryDeleted, entry)

-- isEntryInPath :: FilePath -> IndexEntry -> Bool
-- isEntryInPath relPath IndexEntry{..} = iePath == relPath || (relPath ++ "/") `isPrefixOf` iePath
--
-- filterEntriesUnderPath :: IndexEntries -> FilePath -> IndexEntries
-- filterEntriesUnderPath entries relPath = runST $ do
--   mEntries <- V.unsafeThaw entries
--   startIdx <- VS.binarySearchLBy (\a _ -> compare (iePath a) relPath) mEntries (error "unused")
--
--   let subVec = V.drop startIdx entries
--   let matching = V.takeWhile (isEntryInPath relPath) subVec
--   return matching

findEntryByPath :: IndexEntries -> FilePath -> Maybe IndexEntry
findEntryByPath entries relPath = runST $ do
  mEntries <- V.unsafeThaw entries
  startIdx <- VS.binarySearchBy (\a _ -> compare (iePath a) relPath) mEntries (error "_")
  let found = entries V.! startIdx
  if iePath found == relPath
    then return $ Just found
    else return Nothing

data TreeIndexFold = OnlyInTree (WorkTreePath, TreeItem) | InBoth TreeItem IndexEntry | OnlyInIndex IndexEntry deriving (Show)

treeIndexFoldlM ::
  forall a.
  [(WorkTreePath, TreeItem)] ->
  IndexEntries ->
  a ->
  (a -> TreeIndexFold -> WithRepository a) ->
  WithRepository a
treeIndexFoldlM treeItems indexEntries startAcc fun = work treeItems 0 startAcc
 where
  entriesLen = length indexEntries
  work :: [(WorkTreePath, TreeItem)] -> Int -> a -> WithRepository a

  -- index entries depleted
  work items idx acc | idx >= entriesLen = do
    foldlM fun acc (OnlyInTree <$> items)
  -- tree items depleted
  work [] idx acc = do
    foldlM fun acc (OnlyInIndex <$> V.drop idx indexEntries)
  work items@(item : itemsTail) idx acc = do
    let entry = indexEntries `V.unsafeIndex` idx
    case compare (fst item) (iePath entry) of
      LT -> do
        newAcc <- fun acc $ OnlyInTree item
        work itemsTail idx newAcc
      EQ -> do
        newAcc <- fun acc $ InBoth (snd item) entry
        work itemsTail (idx + 1) newAcc
      GT -> do
        newAcc <- fun acc $ OnlyInIndex entry
        work items (idx + 1) newAcc

data TreeIndexDiff = DiffOnlyInTree (WorkTreePath, TreeItem) | DiffModified TreeItem IndexEntry | DiffSame TreeItem IndexEntry | DiffOnlyInIndex IndexEntry deriving (Show)

treeIndexDiffFoldlM ::
  forall a.
  [(WorkTreePath, TreeItem)] ->
  IndexEntries ->
  Bool ->
  a ->
  (a -> TreeIndexDiff -> WithRepository a) ->
  WithRepository a
treeIndexDiffFoldlM treeItems indexEntries cached startAcc fun = treeIndexFoldlM treeItems indexEntries startAcc $
  \acc i -> case i of
    OnlyInTree item -> fun acc $ DiffOnlyInTree item
    OnlyInIndex entry -> fun acc $ DiffOnlyInIndex entry
    InBoth item entry -> do
      newHash <- if cached then return $ Just $ ieObjHash entry else getEntryHash entry
      case newHash of
        Just hash ->
          if tiHash item == hash
            then fun acc $ DiffSame item entry
            else fun acc $ DiffModified item entry
        Nothing -> fun acc $ DiffOnlyInTree (iePath entry, item)
