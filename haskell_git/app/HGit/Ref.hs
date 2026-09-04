module HGit.Ref where

import Data.Foldable.Extra (findM)
import qualified Data.List as List
import HGit.Repository (WithRepository, gitPath, gitPath')
import HGit.Types (Hash, asciiToHash)
import HGit.Utils
import Relude
import System.FilePath ((</>))
import qualified UnliftIO.Directory as Dir

canonicalizeSymRef :: FilePath -> WithRepository FilePath
canonicalizeSymRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return path
    Just ref -> asks gitPath [ref] >>= canonicalizeSymRef

followRef :: FilePath -> WithRepository Hash
followRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return $ asciiToHash refOrHead
    Just ref -> asks gitPath [ref] >>= followRef

resolveRef :: FilePath -> WithRepository (Maybe Hash)
resolveRef name = do
  let relPaths =
        [ name
        , "refs" </> name
        , "refs" </> "tags" </> name
        , "refs" </> "heads" </> name
        , "refs" </> "remotes" </> name
        , "refs" </> "remotes" </> name </> "HEAD"
        ]

  paths <- mapM gitPath' relPaths
  found <- findM Dir.doesFileExist paths
  case found of
    Nothing -> return Nothing
    Just path -> Just <$> followRef path

collectRefs :: WithRepository [(Hash, String)]
collectRefs = do
  headsPath <- gitPath ["refs", "heads"]
  files <- Dir.listDirectory headsPath
  heads <- forM files $ \name -> do
    hash <- followRef $ headsPath </> name
    return (hash, toString $ "refs/heads/" <> name)

  pass -- TODO: tags and packed-refs
  return heads
