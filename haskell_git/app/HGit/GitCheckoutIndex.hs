module HGit.GitCheckoutIndex (gitCheckoutIndex, CheckoutIndexOptions (..)) where

import Control.Monad.ST (runST)
import qualified Data.Vector as V
import HGit.Index (Index (idxEntries), IndexEntries, IndexEntry (..), findEntryByPath, isEntryModified, readIndex)
import HGit.Object (Object (objPayload), readObj)
import HGit.Repository (WorkTreePath, runWithFoundRepo, toWorktreePath, worktreePath')
import Relude
import System.FilePath (takeDirectory)
import qualified UnliftIO.Directory as Dir

data CheckoutIndexOptions = CheckoutIndexOptions {optFiles :: [String]}

collectEntries :: (MonadIO m) => IndexEntries -> [FilePath] -> m IndexEntries
collectEntries entries paths = do
  flip V.mapMaybeM (V.fromList paths) $ \path -> do
    let matching = findEntryByPath entries path
    when (isNothing matching) $ putStrLn $ path <> ": is not in the cache"
    return matching

gitCheckoutIndex :: CheckoutIndexOptions -> IO ()
gitCheckoutIndex CheckoutIndexOptions{..} = runWithFoundRepo $ do
  worktreePaths <- (snd <$>) <$> mapM toWorktreePath optFiles

  idx <- readIndex

  entries <-
    if null worktreePaths
      then return $ idxEntries idx
      else collectEntries (idxEntries idx) worktreePaths

  forM_ entries $ \entry -> do
    path <- worktreePath' $ iePath entry
    fileExists <- Dir.doesFileExist path
    if fileExists
      then
        putStrLn $ iePath entry <> " already exists, no checkout"
      else do
        obj <- readObj (ieObjHash entry)
        Dir.createDirectoryIfMissing True (takeDirectory path)
        writeFileLBS path $ objPayload obj
