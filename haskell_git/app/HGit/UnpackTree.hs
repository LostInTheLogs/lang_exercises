module HGit.UnpackTree (unpackTree, UnpackTreeOpts (..)) where

import qualified Data.Vector as V
import HGit.FindObject (findAndCoerceToTree)
import HGit.GitDiffIndex (diffTreeIndex)
import HGit.Index (Index (..), IndexEntries, IndexEntry (..), TreeIndexDiff (..), TreeIndexFold (..), isEntryModified, makeEntryAndStat, readIndex, treeIndexDiffFoldlM, treeIndexFoldlM, writeIndex)
import HGit.Object (Object (..), readObj)
import HGit.Repository (Repository (..), WithRepository (WithRepository), WorkTreePath, gitPath, runWithFoundRepo, worktreePath')
import HGit.Tree (FlattenedTree, TreeItem (..), flattenTree, objToTree)
import HGit.Utils
import Relude
import qualified System.FilePath as Path
import qualified UnliftIO.Directory as Dir

-- | Returns untracked files that would be overwritten by checkout
findUntrackedConflicts :: FlattenedTree -> IndexEntries -> WithRepository [WorkTreePath]
findUntrackedConflicts tree entries =
  treeIndexFoldlM tree entries [] $ \acc x -> case x of
    OnlyInTree (path, _) -> do
      conflict <- Dir.doesFileExist =<< worktreePath' path
      return $ if conflict then path : acc else acc
    _ -> return acc

findTrackedConflicts :: IndexEntries -> WithRepository [TreeIndexDiff]
findTrackedConflicts entries = do
  tree <- flattenTree =<< findAndCoerceToTree "HEAD"
  diffTreeIndex tree entries False

data UnpackTreeOpts = UnpackTreeOpts {utoCheckConflicts :: Bool}

checkConflicts :: Index -> FlattenedTree -> WithRepository ()
checkConflicts index flattenedTree = do
  untrackedConflicts <- findUntrackedConflicts flattenedTree (idxEntries index)
  trackedConflicts <- findTrackedConflicts (idxEntries index)

  unless (null trackedConflicts) $ do
    putTextLn "error: Your local changes to the following files would be overwritten by checkout:"
    forM_ (formatTrackedConflict <$> trackedConflicts) $ \x -> putStrLn ("\t" <> x)
    putTextLn "Please commit your changes or stash them before you switch branches."
    putTextLn ""

  unless (null untrackedConflicts) $ do
    putTextLn "error: The following untracked working tree files would be overwritten by checkout:"
    forM_ untrackedConflicts $ \x -> putStrLn ("\t" <> x)
    putTextLn "Please move or remove them before you switch branches."
    putTextLn ""

  unless (null trackedConflicts && null untrackedConflicts) $ do
    putTextLn "Aborting"
    exitFailure

formatTrackedConflict :: TreeIndexDiff -> WorkTreePath
formatTrackedConflict x =
  case x of
    DiffOnlyInIndex entry -> iePath entry
    DiffOnlyInTree (path, _) -> path
    DiffModified _ entry -> iePath entry
    _ -> error "unexpected TreeIndexDiff"

unpackTree :: UnpackTreeOpts -> Index -> FlattenedTree -> WithRepository ()
unpackTree UnpackTreeOpts{..} index flattenedTree = do
  when utoCheckConflicts $ checkConflicts index flattenedTree

  (removedRev, newEntriesRev) <- treeIndexDiffFoldlM flattenedTree (idxEntries index) False ([], []) $ \(rms, entries) a ->
    case a of
      DiffOnlyInTree (wPath, item) -> do
        realPath <- worktreePath' wPath
        Dir.createDirectoryIfMissing True (Path.takeDirectory realPath)

        blob <- readObj $ tiHash item
        writeFileLBS realPath $ objPayload blob

        newEntry <- makeEntryAndStat wPath realPath (tiHash item) (tiMode item)
        return (rms, newEntry : entries)
      DiffModified item entry -> do
        realPath <- worktreePath' $ iePath entry
        Dir.createDirectoryIfMissing True (Path.takeDirectory realPath)

        blob <- readObj $ tiHash item
        writeFileLBS realPath $ objPayload blob

        newEntry <- makeEntryAndStat (iePath entry) realPath (tiHash item) (tiMode item)
        return (rms, newEntry : entries)
      DiffOnlyInIndex entry -> do
        realPath <- worktreePath' $ iePath entry

        exists <- Dir.doesFileExist realPath
        when exists $ Dir.removeFile realPath
        return (realPath : rms, entries)
      DiffSame _ _ -> return (rms, entries)

  let toRemove = reverse removedRev
  let newEntries = reverse newEntriesRev

  untilM null toRemove $ \files -> do
    let candidates = distinctSorted . Path.takeDirectory <$> files
    let foldFun :: [FilePath] -> FilePath -> WithRepository [FilePath]
        foldFun acc path = do
          worktree <- Path.addTrailingPathSeparator <$> asks repoWorktree
          let inRepo = worktree `isPrefixOf` path
          isEmpty <- null <$> Dir.listDirectory path
          if inRepo && isEmpty
            then do
              Dir.removeDirectory path
              return $ path : acc
            else return acc
    reverse <$> foldlM foldFun [] candidates

  let newIdx = index{idxEntries = V.fromList newEntries}
  writeIndex newIdx
