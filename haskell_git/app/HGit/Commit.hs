module HGit.Commit (
  readCommit,
  objToCommit,
  writeCommit,
  oneLineLong,
  oneLineShort,
  Commit (..),
) where

import qualified Control.Monad.Writer as W
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.ByteString.Lazy ((<?>))
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HGit.Object (Hash, ObjType (CommitObj), Object (..), asciiHashParser, makeObject, readObj, readObjOfType, writeObj)
import HGit.Repository (Repository, WithRepository, gitPath)
import HGit.Tree (Tree (treeHash))
import HGit.Utils (fReadStrLine, runParserUnsafe, throwErr)
import Relude

data Commit = Commit
  { commitHash :: Hash -- 20 byte
  , commitTree :: Hash -- 20 byte
  , commitParents :: [Hash] -- 20 byte
  , commitAuthor :: BS.ByteString
  , commitCommitter :: BS.ByteString
  , commitHeaderRest :: BS.ByteString
  , commitMsg :: BS.ByteString
  }
  deriving (Show, Eq)

commitBuilder :: Commit -> B.Builder
commitBuilder Commit{..} = W.execWriter $ do
  W.tell $ lnBuilder "tree" (show commitTree)
  W.tell $ foldMap (lnBuilder "parent" . show) commitParents
  W.tell $ lnBuilder "author" commitAuthor
  W.tell $ lnBuilder "committer" commitCommitter
  W.tell $ B.byteString commitHeaderRest
  W.tell $ B.char7 '\n'
  W.tell $ B.byteString commitMsg
  unless (BSC8.last commitMsg == '\n') $ W.tell $ B.char7 '\n'
 where
  lnBuilder header raw = do
    TE.encodeUtf8Builder header <> B.char7 ' ' <> B.byteString raw <> B.char7 '\n'

commitParser :: Hash -> A.Parser Commit
commitParser commitHash = do
  commitTree <- lineParser "tree" asciiHashParser

  commitParents <- A.many' $ lineParser "parent" asciiHashParser

  commitAuthor <- lineParser "author" $ A8.takeTill (== '\n')
  commitCommitter <- lineParser "committer" $ A8.takeTill (== '\n')

  commitHeaderRest <- restHeaderParser
  commitMsg <- A.takeByteString

  return Commit{..}
 where
  lineParser name parser = (A.string name *> A8.char8 ' ' *> parser <* A8.char8 '\n') <?> show name

  restHeaderParser = restHeaderParserRec mempty <?> "restHeader"
  restHeaderParserRec acc = do
    isNL <- A.option False (True <$ A8.char8 '\n')
    if isNL
      then pure acc
      else do
        chunk <- A8.takeTill (== '\n')
        nl <- A.take 1
        restHeaderParserRec (acc <> chunk <> nl)

objToCommit :: Object -> Commit
objToCommit Object{..} = runParserUnsafe (commitParser objHash) objPayload

readCommit :: Hash -> WithRepository Commit
readCommit hash = objToCommit <$> readObjOfType CommitObj hash

writeCommit :: Commit -> WithRepository Commit
writeCommit commitUnfinished = do
  let payload = B.toLazyByteString $ commitBuilder commitUnfinished
  let obj = makeObject payload CommitObj

  let commit = commitUnfinished{commitHash = objHash obj}

  writeObj obj
  return commit

oneLineShort :: Commit -> Text
oneLineShort Commit{..} = do
  let hash = toText $ take 7 (show commitHash)
  let msg = decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg

oneLineLong :: Commit -> Text
oneLineLong Commit{..} = do
  let hash = show commitHash
  let msg = decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg
