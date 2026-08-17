{-# LANGUAGE BinaryLiterals #-}

module HGit.ObjectCoerce (
  coerceObjTo,
  findAndCoerceObj,
) where

import HGit.Commit (Commit (..), objToCommit)
import HGit.Object (ObjType (..), Object (..), findObject, readObj, readObjOfType)
import HGit.Repository (Repository, WithRepository)
import Relude

coerceObjTo :: ObjType -> Object -> WithRepository Object
coerceObjTo toType obj
  | objType obj == toType = return obj
  | objType obj == CommitObj && toType == TreeObj = do
      let Commit{commitTree = treeHash} = objToCommit obj
      readObjOfType TreeObj treeHash
  | otherwise = return obj

findAndCoerceObj :: ObjType -> Text -> WithRepository Object
findAndCoerceObj oType ref = coerceObjTo oType =<< readObj =<< findObject ref
