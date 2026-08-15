{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Index (
  readIndex,
  isEntryModified,
  getEntryHash,
  getStatData,
  Index (..),
  IndexEntry (..),
  FileMode (..),
) where

import Control.Applicative (many)
import Control.Monad (when)
import qualified Data.Attoparsec.Binary as AB
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.Lazy as A
import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String
import Data.Word (Word32)
import Debug.Trace (trace)
import HGit.Object (Hash, ObjType (BlobObj), Object (objHash), byteHashParser, getFileHash, makeObject)
import HGit.Repository (Repository, repoPath, worktreePath)
import HGit.Tree (FileMode (..))
import HGit.Utils (nameParser, runParserUnsafe, throwErr)
import qualified System.Directory as Dir
import qualified System.Posix.Files as Files

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

data IndexEntry = IndexEntry
  { ieStat :: FileStat
  , ieMode :: FileMode
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
  _flags <- AB.anyWord16be
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

indexParser :: A.Parser Index
indexParser = nameParser "indexParser" $ do
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

{- | checks if the file from the entry has changed
for git ls-files -m
-}
isEntryModified :: Repository -> IndexEntry -> IO Bool
isEntryModified repo entry = do
  newHash <- getEntryHash repo entry
  case newHash of
    Just hash -> return $ hash /= ieObjHash entry
    Nothing -> return True

getEntryHash :: Repository -> IndexEntry -> IO (Maybe Hash)
getEntryHash repo entry = do
  let filePath = worktreePath repo [iePath entry]
  fileExists <- Dir.doesFileExist filePath
  if fileExists
    then do
      stat <- getStatData filePath
      if ieStat entry == stat
        then return $ Just $ ieObjHash entry
        else do Just <$> getFileHash filePath
    else return Nothing
