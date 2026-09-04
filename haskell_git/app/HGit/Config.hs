{-# LANGUAGE TemplateHaskell #-}

module HGit.Config (readConfig, Config) where

import qualified Data.HashMap.Lazy as Map
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified FlatParse.Basic as FP
import HGit.FindObject (findAndCoerceToTree, findObject)
import HGit.Object (ObjType (CommitObj), Object (..), readObjOfType)
import HGit.Repository (WithRepository (WithRepository), gitPath, runWithFoundRepo)
import HGit.Utils
import Relude
import System.FilePath
import qualified UnliftIO.Directory as Dir

{-
FILES
By default, git config will read configuration options from multiple files:
\$(prefix)/etc/gitconfig
  System-wide configuration file.

\$XDG_CONFIG_HOME/git/config, ~/.gitconfig
  User-specific configuration files. When the XDG_CONFIG_HOME environment variable is not set or empty, $HOME/.config/ is used as $XDG_CONFIG_HOME.

  These are also called "global" configuration files. If both files exist, both files are read in the order given above.

\$GIT_DIR/config
  Repository specific configuration file.

\$GIT_DIR/config.worktree
  This is optional and is only searched when extensions.worktreeConfig is present in $GIT_DIR/config.

You may also provide additional configuration parameters when running any git command by using the -c option. See git(1) for details.

Options will be read from all of these files that are available. If the global or the system-wide configuration files are missing or unreadable they will be ignored. If the repository configuration file is  missing
or unreadable, git config will exit with a non-zero error code. An error message is produced if the file is unreadable, but not if it is missing.

The files are read in the order given above, with last value found taking precedence over values read earlier. When multiple values are taken then all values of a key from all files will be used.

By default, options are only written to the repository specific configuration file. Note that this also affects options like set and unset. git config will only ever change one file at a time.

You can limit which configuration sources are read from or written to by specifying the path of a file with the --file option, or by specifying a configuration scope with --system, --global, --local, or --worktree.
For more, see the section called “OPTIONS” above.

SCOPES
Each configuration source falls within a configuration scope. The scopes are:

system
    $(prefix)/etc/gitconfig

global
    $XDG_CONFIG_HOME/git/config

    ~/.gitconfig

local
    $GIT_DIR/config

worktree
    $GIT_DIR/config.worktree

command
    GIT_CONFIG_{COUNT,KEY,VALUE} environment variables (see the section called “ENVIRONMENT” below)

    the -c option

With the exception of command, each scope corresponds to a command line option: --system, --global, --local, --worktree.

When  reading  options, specifying a scope will only read options from the files within that scope. When writing options, specifying a scope will write to the files within that scope (instead of the repository spe‐
cific configuration file). See the section called “OPTIONS” above for a complete description.

Most configuration options are respected regardless of the scope it is defined in, but some options are only respected in certain scopes. See the respective option’s documentation for the full details.

Protected configuration
Protected configuration refers to the system, global, and command scopes. For security reasons, certain options are only respected when they are specified in protected configuration, and ignored otherwise.

Git treats these scopes as if they are controlled by the user or a trusted administrator. This is because an attacker who controls these scopes can do substantial harm without using Git, so it is assumed  that  the
user’s environment protects these scopes against attackers. -}

untilNewline :: Parser ()
untilNewline = FP.skipMany $ FP.satisfy (/= '\n')

skipToNextToken :: Parser ()
skipToNextToken =
  $( FP.switch
      [|
        case _ of
          "\n" -> skipToNextToken
          " " -> skipToNextToken
          "\t" -> skipToNextToken
          "#" -> untilNewline <* $(FP.char '\n') <* skipToNextToken
          ";" -> untilNewline <* $(FP.char '\n') <* skipToNextToken
          _ -> pass
        |]
   )

whitespaceOrComment :: Parser ()
whitespaceOrComment =
  $( FP.switch
      [|
        case _ of
          " " -> whitespaceOrComment
          "\t" -> whitespaceOrComment
          "#" -> untilNewline
          ";" -> untilNewline
          _ -> pass
        |]
   )

whitespace :: Parser ()
whitespace =
  $( FP.switch
      [|
        case _ of
          " " -> whitespace
          "\t" -> whitespace
          _ -> pass
        |]
   )

keyValFParser :: Parser (Text, NonEmpty Text)
keyValFParser = do
  key <- T.toLower . toText <$> FP.some keyChar <* whitespace
  val <- FP.branch $(FP.char '=') valEq (return "true")
  _ <- skipToNextToken
  return (key, NE.singleton val)
 where
  keyChar = FP.satisfy (\c -> FP.isLatinLetter c || FP.isDigit c || c == '-' || c == '_')

  valEq = whitespace *> FP.branch $(FP.char '"') (toText <$> valStrRest) (T.strip . toText <$> valRawRest)

  escapeChar =
    $( FP.switch
        [|
          case _ of
            "\\" -> return '\\'
            "\"" -> return '"'
            "n" -> return '\n'
            "t" -> return '\t'
            "b" -> return '\b'
            "\n" -> FP.anyChar
          |]
     )

  valRawRest =
    $( FP.switch
        [|
          case _ of
            "\n" -> return ""
            "#" -> "" <$ untilNewline <* $(FP.char '\n')
            ";" -> "" <$ untilNewline <* $(FP.char '\n')
            "\\" -> (:) <$> escapeChar <*> valRawRest
            _ -> (:) <$> FP.anyChar <*> valRawRest
          |]
     )

  valStrRest =
    $( FP.switch
        [|
          case _ of
            "\n" -> return ""
            "\"" -> "" <$ whitespaceOrComment <* $(FP.char '\n')
            "\\" -> (:) <$> escapeChar <*> valStrRest
            _ -> (:) <$> FP.anyChar <*> valStrRest
          |]
     )

type HeaderKey = (Text, Text)
type Section = HashMap Text (NonEmpty Text)
type Config = Map.HashMap HeaderKey Section

sectionFParser :: Parser (HeaderKey, Section)
sectionFParser = do
  _ <- $(FP.char '[') <* whitespace
  header <- T.toLower . toText <$> FP.some headerChar <* whitespace
  subheader <- toText <$> FP.branch $(FP.char '"') subheaderRest (return "")
  _ <- whitespace *> $(FP.char ']') <* skipToNextToken

  kv <- FP.some keyValFParser
  let kvMap = Map.fromListWith (flip (<>)) kv

  return ((header, subheader), kvMap)
 where
  headerChar = FP.satisfy (\c -> FP.isLatinLetter c || FP.isDigit c || c == '-' || c == '.')

  subheaderChar = FP.satisfy (\c -> c /= '\0' && c /= '\n')
  subheaderRest =
    $( FP.switch
        [|
          case _ of
            "\"" -> return ""
            "\\" -> (:) <$> subheaderChar <*> subheaderRest
            _ -> (:) <$> subheaderChar <*> subheaderRest
          |]
     )

configFParser :: Parser Config
configFParser = do
  _ <- skipToNextToken
  sections <- FP.many sectionFParser
  return $ Map.fromList sections

readOneConfig :: (MonadIO m) => FilePath -> m Config
readOneConfig path = do
  contents <- readFileBS path
  return $ runFParserUnsafe configFParser contents

mergeConfigs :: Config -> Config -> Config
mergeConfigs = Map.unionWith mergeSections
 where
  mergeSections :: Section -> Section -> Section
  mergeVals a b = a <> b
  mergeSections = Map.unionWith mergeVals

readConfig :: WithRepository Config
readConfig = do
  home <- Dir.getHomeDirectory
  configHome <- Dir.getXdgDirectory Dir.XdgConfig "git"
  gitdir <- gitPath []

  let paths =
        [ configHome </> "config"
        , home </> ".gitconfig"
        , gitdir </> "config"
        ]

  existingPaths <- filterM Dir.doesFileExist paths
  configs <- mapM readOneConfig existingPaths

  return $ foldl' mergeConfigs Map.empty configs
