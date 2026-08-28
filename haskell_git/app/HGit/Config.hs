{-# LANGUAGE TemplateHaskell #-}

module HGit.Config (readConfig, Config) where

import qualified Data.HashMap.Lazy as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified FlatParse.Basic as FP
import HGit.FindObject (findAndCoerceToTree, findObject)
import HGit.Object (ObjType (CommitObj), Object (..), readObjOfType)
import HGit.Repository (WithRepository (WithRepository), gitPath, runWithFoundRepo)
import HGit.Utils
import Relude
import System.FilePath
import qualified UnliftIO.Directory as Dir

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

keyValFParser = do
  key <- T.toLower . toText <$> FP.some keyChar <* whitespace
  val <- FP.branch $(FP.char '=') valEq (return "true")
  _ <- skipToNextToken
  return (key, [val])
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
type SectionValues = HashMap Text [Text]
type Config = Map.HashMap HeaderKey SectionValues

-- sectionFParser :: Parser String
sectionFParser :: Parser (HeaderKey, SectionValues)
sectionFParser = do
  _ <- $(FP.char '[') <* whitespace
  header <- T.toLower . toText <$> FP.some headerChar <* whitespace
  subheader <- toText <$> FP.branch $(FP.char '"') subheaderRest (return "")
  _ <- whitespace *> $(FP.char ']') <* skipToNextToken

  kv <- FP.some keyValFParser
  let kvMap = Map.fromListWith (flip (++)) kv

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

readConfig :: WithRepository Config
readConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home </> ".gitconfig"
  contents <- readFileBS cfgPath
  return $ runFParserUnsafe configFParser contents
