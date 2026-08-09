{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import HGit.GitAdd
import HGit.GitCatFile
import HGit.GitHashObject
import HGit.GitInit
import HGit.Object

import Control.Monad (join)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

objTypeReader :: ReadM ObjType
objTypeReader = maybeReader $ \str -> maybe Nothing Just (strToObjType str)

-- import System.Directory

-- addParser :: Parser (IO ())
-- addParser =
--   runAdd <$> do
--     optFile <- argument str (metavar "FILE" <> help "File path to add")
--     pure (AddOptions{..})

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
        <> command "cat-file" (info hashObjectParser (progDesc "provide contents or details of repository objects"))
