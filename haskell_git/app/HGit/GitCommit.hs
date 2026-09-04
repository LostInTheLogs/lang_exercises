module HGit.GitCommit (gitCommit, CommitOptions (..)) where

import qualified Data.HashMap.Lazy as Map
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TimeP
import qualified Data.Vector as V
import HGit.Commit (Commit (..), Person (..), oneLineLong, readCommit, writeCommit)
import HGit.Config (readConfig)
import HGit.FindObject (findAndCoerceToTree, findObject)
import HGit.Index (FileMode (Directory), Index (idxEntries), IndexEntry (..), readIndex)
import HGit.Object (ObjType (CommitObj), Object (..), readObjOfType)
import HGit.Ref (resolveRef)
import HGit.Repository (WithRepository (WithRepository), WorkTreePath, gitPath, runWithFoundRepo)
import HGit.Tree (Tree (..), TreeItem (..), writeTree)
import HGit.Types (zeroHash)
import HGit.Utils
import Relude
import qualified System.FilePath as Path
import Text.Printf (printf)

data CommitOptions = CommitOptions {optMessage :: Text}

initDef :: [a] -> [a] -> [a]
initDef def xs = maybe def init (nonEmpty xs)

data TrI = TrFile | TrDir [TrItem] deriving (Show)
data TrItem = TrItem String TrI deriving (Show)

indexToTree :: Index -> WithRepository Tree
indexToTree index = writeTree . snd =<< go [] 0 []
 where
  entries = idxEntries index
  entrySplitPaths = NE.fromList . Path.splitDirectories . iePath <$> entries
  entryDirsFiles = (\sp -> (init sp, last sp)) <$> entrySplitPaths
  entriesLen = length entries
  go :: [String] -> Int -> [TreeItem] -> WithRepository (Int, [TreeItem])
  go _ idx items | idx >= entriesLen = return (idx, reverse items)
  go curDirs idx items = do
    let entry = entries `V.unsafeIndex` idx
    let splitPath = NE.fromList $ Path.splitDirectories (iePath entry)
    let (dirs, file) = entryDirsFiles `V.unsafeIndex` idx

    case List.stripPrefix curDirs dirs of
      -- next item
      Just [] -> do
        let newItems = TreeItem (ieMode entry) file (ieObjHash entry) : items
        go curDirs (idx + 1) newItems
      -- subfolder  TODO: submodules
      Just (folder : _) -> do
        (newIdx, subItems) <- go (curDirs ++ [folder]) idx []
        newTree <- writeTree subItems
        -- TODO: write the tree obj

        let newItems = TreeItem Directory folder (treeHash newTree) : items
        go curDirs newIdx newItems
      -- next folder
      Nothing -> do
        return (idx, reverse items)

getGitTimestamp :: (MonadIO m) => m (Int64, ByteString)
getGitTimestamp = liftIO $ do
  time <- Time.getZonedTime
  posixSecs <- round <$> TimeP.getPOSIXTime
  let tz = Time.formatTime Time.defaultTimeLocale "%z" time
  return (posixSecs, encodeUtf8 tz)

gitCommit :: CommitOptions -> IO ()
gitCommit CommitOptions{..} = runWithFoundRepo $ do
  let message = T.strip optMessage

  when (T.null message) $ do
    putTextLn "Empty commit message."
    exitFailure

  index <- readIndex
  tree <- indexToTree index

  parent <- readCommit =<< findObject "HEAD"

  when (commitTree parent == treeHash tree) $ do
    putTextLn "No changes added to commit."
    exitFailure

  config <- readConfig
  let userSection = config Map.! ("user", "")
  let user = last $ userSection Map.! "name"
  let email = last $ userSection Map.! "email"

  (tSecs, tTz) <- getGitTimestamp
  let committer =
        Person
          { pName = encodeUtf8 user
          , pEmail = encodeUtf8 email
          , pTimeSecs = tSecs
          , pTimeTz = tTz
          }

  commit <-
    writeCommit
      Commit
        { commitTree = treeHash tree
        , commitParents = [commitHash parent]
        , commitMsg = encodeUtf8 message
        , commitHeaderRest = ""
        , commitHash = zeroHash
        , commitCommitter = committer
        , commitAuthor = committer
        }

  headFile <- resolveRef =<< gitPath ["HEAD"]
  writeFile headFile $ show (commitHash commit)

  putTextLn $ oneLineLong commit
