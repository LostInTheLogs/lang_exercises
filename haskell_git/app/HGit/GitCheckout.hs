module HGit.GitCheckout (gitCheckout, CheckoutOptions (..)) where

import HGit.Repository (runWithFoundRepo)
import Relude

-- error: The following untracked working tree files would be overwritten by checkout:
-- 	haskell_git/test
-- Please move or remove them before you switch branches.
-- Aborting

data CheckoutOptions = CheckoutOptions {optBranch :: String}

gitCheckout :: CheckoutOptions -> IO ()
gitCheckout CheckoutOptions{..} = runWithFoundRepo $ do
  putStrLn "checkout"

-- stage all files from index for deletion, if not clean abort
-- get all files from the `branch` tree, convert to index entries
-- untracked_from_tree = tree - index
-- if one of the untracked_from_tree files currently exists on disk, abort
-- delete all files staged for deletion ,delete directories if empty and were tracked, somehow
-- switch out index entries
-- checkout the index
