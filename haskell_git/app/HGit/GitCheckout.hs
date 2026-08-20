module HGit.GitCheckout (gitCheckout, CheckoutOptions (..)) where

import HGit.FindObject (findAndCoerceObj, findBranch, findObject)
import HGit.Object (ObjType (..))
import HGit.Repository (gitPath, runWithFoundRepo)
import HGit.Tree (flattenTree, objToTree)
import Relude
import qualified UnliftIO.Directory as Dir

-- error: The following untracked working tree files would be overwritten by checkout:
-- 	haskell_git/test
-- Please move or remove them before you switch branches.
-- Aborting

data CheckoutOptions = CheckoutOptions {optBranch :: Text}

gitCheckout :: CheckoutOptions -> IO ()
gitCheckout CheckoutOptions{..} = runWithFoundRepo $ do
  let branchRef = "refs/heads/" <> optBranch

  branchExists <- Dir.doesFileExist =<< gitPath [toString branchRef]
  newHead <-
    if branchExists
      then return $ "ref: " <> branchRef
      else show <$> findObject optBranch

  tree <- objToTree <$> findAndCoerceObj TreeObj optBranch
  flattened <- flattenTree tree
  forM_ flattened $ print . fst

  putTextLn newHead

-- get all files from the `branch` tree, convert to index entries
-- untracked_from_tree = tree - index
-- if one of the untracked_from_tree files currently exists on disk, abort
-- delete all files in index, if already deleted ignore, delete directories if empty and were tracked, somehow
-- switch out index entries
-- checkout the index
