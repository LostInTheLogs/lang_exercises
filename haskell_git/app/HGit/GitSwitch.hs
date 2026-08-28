module HGit.GitSwitch (gitSwitch, SwitchOptions (..), setHeadToBranch) where

import HGit.FindObject (findAndCoerceToTree, findObject)
import HGit.Index (readIndex)
import HGit.Object (ObjType (CommitObj), Object (..), readObjOfType)
import HGit.Repository (WithRepository (WithRepository), gitPath, runWithFoundRepo)
import HGit.Tree (flattenTree)
import HGit.UnpackTree (UnpackTreeOpts (..), unpackTree)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

data SwitchOptions = SwitchOptions {optBranch :: Text}

setHeadToBranch :: Text -> WithRepository ()
setHeadToBranch ref = do
  let branchRef = "refs/heads/" <> ref
  branchExists <- Dir.doesFileExist =<< gitPath [toString branchRef]
  newHead <-
    if branchExists
      then return $ "ref: " <> branchRef
      else show . objHash <$> (readObjOfType CommitObj =<< findObject ref)

  headPath <- gitPath ["HEAD"]
  writeFileText headPath newHead

gitSwitch :: SwitchOptions -> IO ()
gitSwitch SwitchOptions{..} = runWithFoundRepo $ do
  tree <- findAndCoerceToTree optBranch
  flattened <- flattenTree tree

  idx <- readIndex

  unpackTree UnpackTreeOpts{utoCheckConflicts = True} idx flattened
  setHeadToBranch optBranch
