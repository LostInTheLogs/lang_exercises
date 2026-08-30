module HGit.Commit (
  readCommit,
  objToCommit,
  writeCommit,
  oneLineLong,
  oneLineShort,
  makeCmtQueue,
  cmtQueuePop,
  CommitQueue (),
  Commit (..),
  Person (..),
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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.PQueue.Max as Q
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import HGit.Object (Hash, ObjType (CommitObj), Object (..), makeObject, readObj, readObjOfType, writeObj)
import HGit.Repository (Repository, WithRepository, gitPath)
import HGit.Tree (Tree (treeHash))
import HGit.Types (asciiHashParser, zeroHash)
import HGit.Utils (fReadStrLine, runParserUnsafe, throwErr)
import Relude

data Person = Person {pName :: BS.ByteString, pEmail :: BS.ByteString, pTimeSecs :: Int64, pTimeTz :: ByteString} deriving (Show)

data Commit = Commit
  { commitHash :: Hash -- 20 byte
  , commitTree :: Hash -- 20 byte
  , commitParents :: [Hash] -- 20 byte
  , commitAuthor :: Person
  , commitCommitter :: Person
  , commitHeaderRest :: BS.ByteString
  , commitMsg :: BS.ByteString
  }
  deriving (Show)

instance Eq Commit where
  a == _ | commitHash a == zeroHash = False
  _ == b | commitHash b == zeroHash = False
  a == b = commitHash a == commitHash b

instance Ord Commit where
  compare = comparing (pTimeSecs . commitCommitter)

commitBuilder :: Commit -> B.Builder
commitBuilder Commit{..} = W.execWriter $ do
  W.tell $ lnBuilder "tree" (show commitTree)
  W.tell $ foldMap (lnBuilder "parent" . show) commitParents
  W.tell $ personBuilder "author" commitAuthor
  W.tell $ personBuilder "committer" commitCommitter
  W.tell $ B.byteString commitHeaderRest
  W.tell $ B.char7 '\n'
  W.tell $ B.byteString commitMsg
  unless (BSC8.last commitMsg == '\n') $ W.tell $ B.char7 '\n'
 where
  personBuilder header Person{..} = do
    lnBuilder header $ B.byteString pName <> " <" <> B.byteString pEmail <> "> " <> B.int64Dec pTimeSecs <> " " <> B.byteString pTimeTz
  lnBuilder header content = do
    TE.encodeUtf8Builder header <> B.char7 ' ' <> content <> B.char7 '\n'

commitParser :: Hash -> A.Parser Commit
commitParser commitHash = do
  commitTree <- lineParser "tree" asciiHashParser

  commitParents <- A.many' $ lineParser "parent" asciiHashParser

  commitAuthor <- personParser "author"
  commitCommitter <- personParser "committer"

  commitHeaderRest <- restHeaderParser
  commitMsg <- A.takeByteString

  return Commit{..}
 where
  lineParser name parser = (A.string name *> A8.char8 ' ' *> parser <* A8.char8 '\n') <?> show name
  personParser header = lineParser header $ do
    line <- A8.takeTill (== '\n')
    let (namePart, rest) = BSC8.breakEnd (== '<') line
        name = BSC8.strip (BS.dropEnd 1 namePart)
        (email, timePart) = BSC8.span (/= '>') rest
        (secs, spcTz) = fromJust $ BSC8.readInt64 (BS.drop 2 timePart)
    return $ Person{pName = name, pEmail = email, pTimeSecs = secs, pTimeTz = BS.drop 1 spcTz}

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

  let oneLnMsg = T.strip $ T.intercalate " " $ takeWhile (not . T.null) $ lines msg

  hash <> " " <> oneLnMsg

oneLineLong :: Commit -> Text
oneLineLong Commit{..} = do
  let hash = show commitHash
  let msg = decodeUtf8 commitMsg
  let oneLnMsg = T.strip $ T.replace "\n" " " msg
  hash <> " " <> oneLnMsg

type CommitQueue = (Q.MaxQueue Commit, Set Hash)

makeCmtQueue :: [Commit] -> CommitQueue
makeCmtQueue commits = do
  (Q.fromList commits, fromList $ commitHash <$> commits)

cmtQueuePop :: CommitQueue -> WithRepository (Maybe Commit, CommitQueue)
cmtQueuePop (queue, seen) = do
  if Q.null queue
    then return (Nothing, (queue, seen))
    else do
      let (top, newQueue) = Q.deleteFindMax queue

      let newParens = filter (`S.notMember` seen) $ commitParents top
      parentCommits <- mapM readCommit newParens

      let finQueue = flipfoldl' Q.insert newQueue parentCommits
      let finSeen = flipfoldl' S.insert seen newParens
      return (Just top, (finQueue, finSeen))
