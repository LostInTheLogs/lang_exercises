{-# LANGUAGE BinaryLiterals #-}

module HGit.FindObject (
  findObject,
  coerceObjTo,
  findAndCoerceObj,
  findAndCoerceToTree,
) where

import Control.Monad.Extra (firstJustM)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import HGit.Commit (Commit (..), objToCommit)
import HGit.Object (Hash (..), ObjType (..), Object (..), readObj, readObjOfType)
import HGit.Ref
import HGit.Repository (Repository, WithRepository, gitPath)
import HGit.Tree (Tree, objToTree)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

findHash :: Text -> WithRepository (Maybe Hash)
findHash hashText = do
  case Base16.decode (encodeUtf8 hashText) of
    Left _ -> return Nothing
    Right val -> return $ Just $ Hash $ toShort val

-- | get hash from e.g. HEAD
findObject :: Text -> WithRepository Hash
findObject obj = do
  let possibilities =
        [ resolveRef $ toString obj
        , findHash obj
        ]
  res <- firstJustM id possibilities
  case res of
    Just found -> return found
    Nothing -> throwErr "findObject" $ "Couldn't find an object from: " <> obj

coerceObjTo :: ObjType -> Object -> WithRepository Object
coerceObjTo toType obj
  | objType obj == toType = return obj
  | objType obj == CommitObj && toType == TreeObj = do
      let Commit{commitTree = treeHash} = objToCommit obj
      readObjOfType TreeObj treeHash
  | otherwise = throwErr "coerceObjTo" "couldn't coerce"

findAndCoerceObj :: ObjType -> Text -> WithRepository Object
findAndCoerceObj oType ref = coerceObjTo oType =<< readObj =<< findObject ref

findAndCoerceToTree :: Text -> WithRepository Tree
findAndCoerceToTree ref = objToTree <$> findAndCoerceObj TreeObj ref
