module HGit.UnpackTree (unpackTree) where

import qualified Data.Vector as V
import HGit.Index (Index (..), IndexEntry (..), TreeIndexDiff (..), makeEntryAndStat, treeIndexDiffFoldlM, writeIndex)
import HGit.Object (Object (..), readObj)
import HGit.Repository (Repository (..), WithRepository, WorkTreePath, worktreePath')
import HGit.Tree (TreeItem (..))
import HGit.Utils
import Relude
import qualified System.FilePath as Path
import qualified UnliftIO.Directory as Dir

unpackTree :: Index -> [(WorkTreePath, TreeItem)] -> WithRepository ()
unpackTree index flattenedTree = do
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
