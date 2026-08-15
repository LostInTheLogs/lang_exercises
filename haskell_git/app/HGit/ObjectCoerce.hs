{-# LANGUAGE BinaryLiterals #-}

module HGit.ObjectCoerce (
  coerceObjTo,
  findAndCoerceObj,
) where

import HGit.Commit (Commit (..), objToCommit)
import HGit.Object (ObjType (..), Object (..), findObject, readObj, readObjOfType)
import HGit.Repository (Repository)

coerceObjTo :: Repository -> ObjType -> Object -> IO Object
coerceObjTo repo toType obj
  | objType obj == toType = return obj
  | objType obj == CommitObj && toType == TreeObj = do
      let Commit{commitTree = treeHash} = objToCommit obj
      readObjOfType repo TreeObj treeHash
  | otherwise = return obj

findAndCoerceObj :: Repository -> ObjType -> String -> IO Object
findAndCoerceObj repo oType ref = coerceObjTo repo oType =<< readObj repo =<< findObject repo ref
