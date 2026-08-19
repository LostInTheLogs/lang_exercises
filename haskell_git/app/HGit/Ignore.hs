module HGit.Ignore (listRepoFilesRecursive) where

import Data.List.Extra (lastDef)
import qualified Data.Text as T
import HGit.Repository (WithRepository, worktreePath)
import Relude
import qualified System.FilePath as Path
import qualified System.FilePattern as FP
import qualified UnliftIO.Directory as Dir

data GitGlobMeta = GitGlobMeta
  { ruleIndex :: Int
  , isNegated :: Bool
  }
  deriving (Show, Eq, Ord)

type GitGlob = (GitGlobMeta, FP.FilePattern)

listRepoFilesRecursive :: FilePath -> WithRepository [FilePath]
listRepoFilesRecursive = listRepoFilesRecursive_ []

-- https://hackage.haskell.org/package/filepattern-0.1.3/docs/System-FilePattern.html#v:stepDone
listRepoFilesRecursive_ :: [FP.Step GitGlobMeta] -> FilePath -> WithRepository [FilePath]
listRepoFilesRecursive_ oldSteps relPath = do
  absPath <- worktreePath [relPath]
  entries <- Dir.listDirectory absPath

  let maybeGitignore = find (== ".gitignore") entries

  newSteps <- case maybeGitignore of
    Just _ -> (: oldSteps) <$> loadGitignore
    Nothing -> return oldSteps

  fmap concat $ forM entries $ \entry -> do
    let relEntry = relPath Path.</> entry
    path <- worktreePath [relEntry]
    isDir <- Dir.doesDirectoryExist path

    let stepped = flip FP.stepApply entry <$> newSteps
    let matches = sort $ concatMap FP.stepDone stepped
    let keep = lastDef True $ isNegated . fst <$> matches

    case (keep, isDir, entry) of
      (False, _, _) -> pure []
      (_, _, ".git") -> pure []
      (_, True, _) -> listRepoFilesRecursive_ newSteps relEntry
      (_, False, _) -> pure [relEntry]
 where
  loadGitignore :: WithRepository (FP.Step GitGlobMeta)
  loadGitignore = do
    path <- worktreePath [relPath, ".gitignore"]
    bytes <- readFileBS path
    let textContent = decodeUtf8With lenientDecode bytes
        rawLines = T.lines textContent

    return $ FP.step $ catMaybes $ zipWith parseLine [0 ..] rawLines

  parseLine :: Int -> Text -> Maybe GitGlob
  parseLine idx line
    | T.null trimmed || "#" `T.isPrefixOf` trimmed = Nothing
    | otherwise =
        Just
          ( GitGlobMeta{ruleIndex = idx, isNegated = isNeg}
          , fixedPattrn
          )
   where
    trimmed = T.strip line
    (isNeg, rawPat) =
      if "!" `T.isPrefixOf` trimmed
        then (True, T.drop 1 trimmed)
        else (False, trimmed)
    strPat = toString rawPat
    fixedPattrn = case strPat of
      -- "/abc" -> "abc"
      '/' : rest -> rest
      -- "**/abc" -> "**/abc"
      '*' : '*' : rest -> rest
      -- "abc" -> "**/abc"
      _ -> "**/" <> strPat
