{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module HGit.GitFetch (gitFetch, FetchOptions (..)) where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Conduit.Combinators as C
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as FP
import HGit.Commit (Commit, CommitQueue, cmtQueuePop, commitHash, makeCmtQueue, readCommit)
import HGit.Object (Hash, strToHash)
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, runWithFoundRepo)
import HGit.Types (asciiHashFParser, byteHashFParser)
import HGit.Utils
import qualified Network.HTTP.Simple as Http
import Network.HTTP.Types (hAccept, hContentType)
import Network.HTTP.Types.Header (hUserAgent)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.Printf (printf)

data FetchOptions = FetchOptions {}

-- https://git-scm.com/docs/http-protocol
-- https://git-scm.com/docs/pack-protocol
-- https://git-scm.com/docs/protocol-common

dropSuffix :: Text -> Text -> Text
dropSuffix suffix txt = fromMaybe txt (T.stripSuffix suffix txt)

normalizeGitUrl :: Text -> Text
normalizeGitUrl urlRaw = do
  let urlNoBS = dropSuffix "/" urlRaw
  if ".git" `T.isSuffixOf` urlNoBS then urlNoBS else urlNoBS <> ".git"

pktLineB :: ByteString -> B.Builder
pktLineB "" = B.byteString "0000"
pktLineB content = do
  let len = printf "%04x" $ 5 + BS.length content
  B.string7 len <> B.byteString content <> B.char7 '\n'

pktLineDecoder :: (Monad m) => ConduitT ByteString ByteString m ()
pktLineDecoder = do
  lenRaw <- takeCE 4 .| foldC
  case lenRaw of
    "0000" -> yield "" *> pktLineDecoder
    "" -> pass
    _ -> do
      let len = runFParserUnsafe FP.anyAsciiHexInt lenRaw
      body <- takeCE (len - 4) .| foldC
      when (BS.null body) $ throwErr "pktLineDecoder" "malformed pktline"
      if BSC8.last body == '\n'
        then
          yield $ BSU.unsafeInit body
        else
          yield body
      pktLineDecoder

pktLineWExtraFP :: Parser a -> Parser b -> Parser (a, b)
pktLineWExtraFP contentFP extraFP = do
  cnt <- FP.isolateToNextNull contentFP
  ext <- extraFP
  return (cnt, ext)

awaitUnsafe :: (HasCallStack) => (Monad m) => ConduitT i o m i
awaitUnsafe = fromJust <$> await

awaitExactly :: (HasCallStack) => (Monad m, Eq i) => i -> ConduitT i o m i
awaitExactly x = do
  val <- fromJust <$> await
  if val == x
    then
      return val
    else throwErr "awaitExactly" "got unexpected data"

infoRefsSmart :: (Monad m) => ConduitT ByteString Void m (ByteString, [(Hash, ByteString)])
infoRefsSmart = do
  _serviceLine <- awaitUnsafe

  _ <- awaitExactly ""

  (firstRef, capabilities) <- parseAwait (pktLineWExtraFP refFP FP.takeRest)

  restRefs <-
    takeWhileC (not . BS.null)
      .| foldMapC (List.singleton . runFParserUnsafe refFP)

  return (capabilities, firstRef : restRefs)
 where
  parseAwait parser = runFParserUnsafe parser <$> awaitUnsafe
  refFP = do
    hash <- asciiHashFParser
    _spc <- $(FP.char ' ')
    name <- FP.takeRest
    return (hash, name)

refDiscovery url = do
  let refsUrl = "GET " <> url <> "/info/refs?service=git-upload-pack"
  req <- liftIO $ Http.parseRequest $ toString refsUrl
  -- response <- Http.httpBS req
  -- let body = Http.getResponseBody response
  -- writeFileBS "/tmp/hgit_info_refs_body" body

  runConduitRes $
    C.sourceFile "/tmp/hgit_info_refs_body"
      -- Http.httpSource initReq getSrc
      .| pktLineDecoder
      .| infoRefsSmart

gitFetch :: FetchOptions -> IO ()
gitFetch FetchOptions{..} = runWithFoundRepo $ do
  let url = normalizeGitUrl "https://github.com/LostInTheLogs/gleam_exercises"

  let uploadPackUrl = "POST " <> url <> "/git-upload-pack"
  -- TODO: common request headers
  uploadPackReqEmpty <-
    liftIO $
      Http.setRequestHeader hUserAgent ["hgit"]
        . Http.setRequestHeader hContentType ["application/x-git-upload-pack-request"]
        . Http.setRequestHeader hAccept ["application/x-git-upload-pack-result"]
        <$> Http.parseRequest (toString uploadPackUrl)

  (capabilities, remoteRefs) <- refDiscovery url
  -- TODO: check if we have the remote refs already

  let wants = map head $ NE.group $ sort $ fst <$> remoteRefs -- TODO: handle HEAD instead of distinct
  let wantsBody = commandBuilder "want" wants
  -- let wantsBody = pktLineB "want dea957cebcdaf965936bf81862070afef8e11f18"

  uniqueRefs <- map head . NE.group . sort <$> collectRefs
  pending <- makeCmtQueue <$> mapM readCommit [strToHash "d774d7c035dad1ba94aec0e999f451cfb3582bb9"] -- more rounds
  -- pending <- makeCmtQueue <$> mapM readCommit [strToHash "44b08a3abb38383ce4313b4fb511d1387de8394b"] -- one commit
  -- pending <- makeCmtQueue <$> mapM readCommit (fst <$> uniqueRefs) -- HEAD
  negotiate uploadPackReqEmpty wantsBody pending

  pass
 where
  getSrc res = do
    -- print (Http.getResponseStatus res, Http.getResponseHeaders res)
    Http.getResponseBody res

  negotiate reqEmpty wantsBody pending = do
    (haves, newPending) <- popN pending 32

    let havesBody = commandBuilder "have" haves

    let reqBody = B.toLazyByteString $ wantsBody <> pktLineB "" <> havesBody <> pktLineB ""
    let req = Http.setRequestBodyLBS reqBody reqEmpty
    print reqBody

    response <- Http.httpBS req
    print response
    let body = Http.getResponseBody response
    print $ BS.length body
    writeFileBS "/home/vodfsh/Downloads/hgit_negotateEmpty.bin" body

  -- putLBSLn reqBody

  commandBuilder cmd = foldMap (\a -> pktLineB (cmd <> " " <> show a))

  popN :: CommitQueue -> Int -> WithRepository ([Hash], CommitQueue)
  popN = go []
   where
    go acc queue 0 = return (reverse acc, queue)
    go acc queue k = do
      popped <- cmtQueuePop queue
      case popped of
        (Nothing, _) -> return (reverse acc, queue)
        (Just cmt, nextQ) -> go (commitHash cmt : acc) nextQ (k - 1)
