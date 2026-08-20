{-# LANGUAGE BinaryLiterals #-}

module HGit.FindObject (
  findObject,
  findBranch,
  coerceObjTo,
  findAndCoerceObj,
) where

import Control.Monad.Extra (firstJustM)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import HGit.Commit (Commit (..), objToCommit)
import HGit.Object (Hash (..), ObjType (..), Object (..), readObj, readObjOfType, strToHash)
import HGit.Repository (Repository, WithRepository, gitPath)
import HGit.Utils
import Relude
import qualified UnliftIO.Directory as Dir

readRef :: FilePath -> WithRepository Hash
readRef path = do
  refOrHead <- fReadStrLine path
  case List.stripPrefix "ref: " refOrHead of
    Nothing -> return $ strToHash refOrHead
    Just ref -> asks gitPath [ref] >>= readRef

findBranch :: FilePath -> WithRepository (Maybe Hash)
findBranch name = do
  path <- asks gitPath ["refs", "heads", name]
  fileExists <- Dir.doesFileExist path
  if fileExists then Just <$> readRef path else return Nothing

findHash :: Text -> WithRepository (Maybe Hash)
findHash hashText = do
  case Base16.decode (encodeUtf8 hashText) of
    Left _ -> return Nothing
    Right val -> return $ Just $ Hash val

-- | get hash from e.g. HEAD
findObject :: Text -> WithRepository Hash
findObject "HEAD" = readRef =<< asks gitPath ["HEAD"]
findObject obj = do
  let possibilities =
        [ findBranch $ toString obj
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
  | otherwise = return obj

findAndCoerceObj :: ObjType -> Text -> WithRepository Object
findAndCoerceObj oType ref = coerceObjTo oType =<< readObj =<< findObject ref
