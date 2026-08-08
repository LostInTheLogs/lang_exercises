{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import HGit.GitAdd
import HGit.GitHashObject
import HGit.GitInit

import Control.Monad (join)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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

main :: IO ()
main = join $ customExecParser parserPrefs (info (parser <**> helper) fullDesc)
 where
  parserPrefs :: ParserPrefs
  parserPrefs = prefs showHelpOnError

  parser :: Parser (IO ())
  parser =
    hsubparser $
      command "init" (info initParser (progDesc "Create an empty git repository"))
        <> command "hash-object" (info hashObjectParser (progDesc "compute object ID "))
