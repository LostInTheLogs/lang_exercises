module HGit.Ref where

import qualified Data.List as List
import HGit.Repository (WithRepository, gitPath)
import HGit.Types (Hash, asciiToHash)
import HGit.Utils
import Relude
import System.FilePath ((</>))
import qualified UnliftIO.Directory as Dir

-- TODO:
-- .git/<refname> (exact path, e.g., HEAD, FETCH_HEAD, ORIG_HEAD)
-- .git/refs/<refname>
-- .git/refs/tags/<refname>
-- .git/refs/heads/<refname>
-- .git/refs/remotes/<refname>
-- .git/refs/remotes/<refname>/HEAD

resolveRef :: FilePath -> WithRepository FilePath
resolveRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return path
    Just ref -> asks gitPath [ref] >>= resolveRef

readRef :: FilePath -> WithRepository Hash
readRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return $ asciiToHash refOrHead
    Just ref -> asks gitPath [ref] >>= readRef

findBranch :: FilePath -> WithRepository (Maybe Hash)
findBranch name = do
  path <- asks gitPath ["refs", "heads", name]
  fileExists <- Dir.doesFileExist path
  if fileExists then Just <$> readRef path else return Nothing

collectRefs :: WithRepository [(Hash, String)]
collectRefs = do
  headsPath <- gitPath ["refs", "heads"]
  files <- Dir.listDirectory headsPath
  heads <- forM files $ \name -> do
    hash <- readRef $ headsPath </> name
    return (hash, toString $ "refs/heads/" <> name)

  pass -- TODO: tags and packed-refs
  return heads
