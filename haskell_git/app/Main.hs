{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import HGit.GitAdd
import HGit.GitCatFile
import HGit.GitDiffIndex
import HGit.GitHashObject
import HGit.GitInit
import HGit.GitLog
import HGit.GitLsFiles
import HGit.GitLsTree

import Control.Monad (join)
import HGit.Object (ObjType, deserializeObjType)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

objTypeReader :: ReadM ObjType
objTypeReader = maybeReader deserializeObjType

initParser :: Parser (IO ())
initParser =
  gitInit <$> do
    optPath <- argument str (value "." <> metavar "PATH" <> help "Path to repository")
    pure (InitOptions{..})

hashObjectParser :: Parser (IO ())
hashObjectParser =
  gitHashObject <$> do
    optPath <- argument str (metavar "PATH" <> help "Path to file")
    optWrite <- switch (short 'w' <> help "Write object to database")

    pure (HashObjectOptions{..})

catFileParser :: Parser (IO ())
catFileParser =
  gitCatFile <$> do
    optType <- argument objTypeReader (metavar "TYPE")
    optObject <- argument str (metavar "OBJECT")
    pure (CatFileOptions{..})

logParser :: Parser (IO ())
logParser =
  gitLog <$> do
    optRef <- argument str (value "HEAD" <> metavar "REF" <> help "Revision to start with")
    pure (LogOptions{..})

lsTreeParser :: Parser (IO ())
lsTreeParser =
  gitLsTree <$> do
    optRef <- argument str (metavar "TREE-ISH" <> help "Tree to print")
    optRecurse <- switch (short 'r' <> help "Recursive")
    pure (LsTreeOptions{..})

lsFilesParser :: Parser (IO ())
lsFilesParser =
  gitLsFiles <$> do
    optModified <- switch (short 'm' <> help "show modified only")
    pure (LsFilesOptions{..})

diffIndexParser :: Parser (IO ())
diffIndexParser =
  gitDiffIndex <$> do
    optTree <- argument str (metavar "TREE-ISH" <> help "Tree to print")
    optCached <- switch (long "cached" <> help "don't consider the work tree at all")
    pure (DiffIndexOptions{..})

main :: IO ()
main = join $ customExecParser parserPrefs (info (parser <**> helper) fullDesc)
 where
  parserPrefs :: ParserPrefs
  parserPrefs = prefs showHelpOnError

  parser :: Parser (IO ())
  parser =
    hsubparser $
      command "init" (info initParser (progDesc "Create an empty git repository"))
        <> command "hash-object" (info hashObjectParser (progDesc "compute object ID"))
        <> command "cat-file" (info catFileParser (progDesc "provide contents or details of repository objects"))
        <> command "log" (info logParser (progDesc "show commit log"))
        <> command "ls-tree" (info lsTreeParser (progDesc "list contents of a tree object"))
        <> command "ls-files" (info lsFilesParser (progDesc "information about files in index/working directory"))
        <> command "diff-index" (info diffIndexParser (progDesc "compare content and mode of blobs between index and repository"))
