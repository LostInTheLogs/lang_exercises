{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Index (
  readIndex,
  isEntryModified,
  Index (..),
  IndexEntry (..),
) where

import Control.Applicative (many)
import Control.Monad (when)
import qualified Data.Attoparsec.Binary as AB
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.Attoparsec.Lazy as A
import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String
import Data.Word (Word32)
import HGit.Object (Hash, ObjType (BlobObj), Object (objHash), byteHashParser, makeObject)
import HGit.Repository (Repository, repoPath, worktreePath)
import HGit.Utils (runParserUnsafe, throwErr)
import qualified System.Posix.Files as Files

data Permissions = PermNorm | PermExec deriving (Show, Eq)

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

data IndexExtension = IndexExtension {extSig :: BS.ByteString} deriving (Show)

data EntryObjType = EntryRegularFile | EntrySymlink | EntryGitlink deriving (Show, Eq)

data IndexEntry = IndexEntry
  { ieStat :: FileStat
  , ieType :: EntryObjType
  , iePerm :: Permissions
  , ieObjHash :: Hash
  , iePath :: FilePath
  }
  deriving (Show, Eq)

data Index = Index
  { idxVersion :: Word32
  , idxEntries :: [IndexEntry]
  , idxExtensions :: [IndexExtension]
  }
  deriving (Show)

extensionParser :: A.Parser IndexExtension
extensionParser = do
  extSig <- A.take 4
  size <- AB.anyWord32be
  _raw <- A.take (fromIntegral size)
  return IndexExtension{..}

indexEntryParser :: A.Parser IndexEntry
indexEntryParser = do
  statDataModified <- (,) <$> AB.anyWord32be <*> AB.anyWord32be
  statMDataModified <- (,) <$> AB.anyWord32be <*> AB.anyWord32be
  statDev <- AB.anyWord32be
  statIno <- AB.anyWord32be
  _ <- AB.word16be 0
  packed <- AB.anyWord16be
  let ieType = case packed `Bits.shiftR` 12 of
        0b1000 -> EntryRegularFile
        0b1010 -> EntrySymlink
        0b1110 -> EntryGitlink
        _ -> throwErr "indexEntryParser" "unknown type"
  let iePerm = case packed .&. 0x1FF of
        0o644 -> PermNorm
        0o755 -> PermExec
        _ -> throwErr "indexEntryParser" "unknown perm"
  statUid <- AB.anyWord32be
  statGid <- AB.anyWord32be
  statSize <- AB.anyWord32be
  ieObjHash <- byteHashParser
  _flags <- AB.anyWord16be
  -- v3+flags <- AB.anyWord16be
  iePath <- BSC8.unpack <$> A8.takeTill (== '\0')
  _ <- A.many1 $ A8.char '\0'

  let ieStat = FileStat{..}
  return (IndexEntry{..})

indexParser :: A.Parser Index
indexParser = do
  _ <- A.string "DIRC"
  idxVersion <- AB.anyWord32be
  when (idxVersion /= 2) $ throwErr "indexParser" "unsupported index version"
  len <- AB.anyWord32be
  idxEntries <- A.count (fromIntegral len) indexEntryParser
  idxExtensions <- many extensionParser
  _hash <- byteHashParser
  _ <- A.endOfInput
  return Index{..}

-- https://github.com/git/git/blob/master/Documentation/gitformat-index.adoc
readIndex :: Repository -> IO Index
readIndex repo = do
  let indexFile = repoPath repo ["index"]
  raw <- BSL.readFile indexFile
  return $ runParserUnsafe indexParser raw

getStatData :: FilePath -> IO FileStat
getStatData path = do
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

getFileHash :: FilePath -> IO Hash
getFileHash path = do
  contents <- BSL.readFile path
  let obj = makeObject contents BlobObj
  return $ objHash obj

{- | checks if the file from the entry has changed
for git ls-files -m
-}
isEntryModified :: Repository -> IndexEntry -> IO Bool
isEntryModified repo IndexEntry{..} = do
  let filePath = worktreePath repo [iePath]
  stat <- getStatData filePath
  if ieStat /= stat
    then (ieObjHash /=) <$> getFileHash filePath
    else return False
