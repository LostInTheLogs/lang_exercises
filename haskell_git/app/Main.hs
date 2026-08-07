{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import HGitLib.GitAdd
import HGitLib.GitInit

import Control.Monad (join)
import Options.Applicative

-- addParser :: Parser (IO ())
-- addParser =
--   runAdd <$> do
--     optFile <- argument str (metavar "FILE" <> help "File path to add")
--     pure (AddOptions{..})

initParser :: Parser (IO ())
initParser =
  gitInit <$> do
    pure (InitOptions{..})

main :: IO ()
main = join $ customExecParser parserPrefs (info (parser <**> helper) fullDesc)
 where
  parserPrefs :: ParserPrefs
  parserPrefs = prefs showHelpOnError

  parser :: Parser (IO ())
  parser =
    hsubparser
      ( command "init" (info initParser (progDesc "Create an empty git repository"))
      -- <> command "add" (info addParser (progDesc "Add a file to the repository"))
      )
