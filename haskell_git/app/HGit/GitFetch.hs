{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module HGit.GitFetch (gitFetch, FetchOptions (..)) where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Conduit.Combinators as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.PQueue.Max as Q
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as FP
import HGit.Commit (Commit (..), CommitQueue, cmtQueuePop, commitHash, makeCmtQueue, readCommit)
import HGit.Object (Hash (..))
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, runWithFoundRepo)
import HGit.Types (asciiHashFParser, asciiToHash, byteHashFParser)
import HGit.Utils
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types as HttpT
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.Printf (printf)

-- import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Conduit.Combinators as CC
-- import Data.Conduit.TQueue (TQueue, newTQueueIO, sinkTQueue, sourceTQueue, writeTQueue)
-- import UnliftIO.Async (Concurrently (..), runConcurrently)

-- -- Router Conduit: accepts Int, routes to either evenQ or oddQ
-- partitionRouter :: TQueue (Maybe Int) -> TQueue (Maybe Int) -> ConduitT Int Void IO ()
-- partitionRouter evenQ oddQ =
--   await >>= \case
--     Just x -> do
--       if even x
--         then liftIO $ atomically $ writeTQueue evenQ (Just x)
--         else liftIO $ atomically $ writeTQueue oddQ (Just x)
--       partitionRouter evenQ oddQ
--     Nothing -> liftIO $ atomically $ do
--       writeTQueue evenQ Nothing
--       writeTQueue oddQ Nothing

-- main :: IO ()
-- main = do
--   -- 1. Create STM Queues for the partitioned streams
--   evenQ <- newTQueueIO
--   oddQ <- newTQueueIO

--   let source = CC.yieldMany [1 .. 10]

--   -- 2. Run the feeder stream and the two downstream conduits concurrently
--   runConcurrently $
--     (,,)
--       <$> Concurrently (runConduit $ source .| partitionRouter evenQ oddQ)
--       <*> Concurrently (runConduit $ sourceTQueue evenQ .| CC.map (\x -> "Even: " ++ show x) .| CC.print)
--       <*> Concurrently (runConduit $ sourceTQueue oddQ .| CC.map (\x -> "Odd: " ++ show x) .| CC.print)

--   return ()

data FetchOptions = FetchOptions {}

-- https://git-scm.com/docs/http-protocol
-- https://git-scm.com/docs/pack-protocol
-- https://git-scm.com/docs/protocol-capabilities
-- https://git-scm.com/docs/gitprotocol-pack

type PktLineData = ByteString

dropSuffix :: Text -> Text -> Text
dropSuffix suffix txt = fromMaybe txt (T.stripSuffix suffix txt)

normalizeGitUrl :: Text -> Text
normalizeGitUrl urlRaw = do
  let urlNoBS = dropSuffix "/" urlRaw
  if ".git" `T.isSuffixOf` urlNoBS then urlNoBS else urlNoBS <> ".git"

pktLineB :: PktLineData -> B.Builder
pktLineB "" = B.byteString "0000"
pktLineB content = do
  let len = printf "%04x" $ 5 + BS.length content
  B.string7 len <> B.byteString content <> B.char7 '\n'

pktLineDecoder :: (Monad m) => ConduitT ByteString PktLineData m ()
pktLineDecoder = do
  lenRaw <- takeCE 4 .| foldC
  case lenRaw of
    "0000" -> yield "" *> pktLineDecoder
    "PACK" -> leftover "PACK" *> pass
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

hgitRequest :: (MonadIO m, ToString a) => a -> m Http.Request
hgitRequest url =
  liftIO $
    Http.setRequestHeader HttpT.hUserAgent ["hgit"]
      <$> Http.parseRequest (toString url)

getSrc :: Http.Response a -> a
getSrc res = do
  if HttpT.ok200 == Http.getResponseStatus res
    then
      Http.getResponseBody res
    else throwErr "getSrc" $ show (res $> "[removed for `show`. TODO: drain the conduit to a bytestring]")

infoRefsSmartS :: (Monad m) => ConduitT PktLineData Void m (ByteString, [(Hash, ByteString)])
infoRefsSmartS = do
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

refDiscovery :: (MonadUnliftIO m) => p -> m (ByteString, [(Hash, ByteString)])
refDiscovery url = do
  -- let refsUrl = "GET " <> url <> "/info/refs?service=git-upload-pack"
  -- req <- liftIO $ Http.parseRequest $ toString refsUrl
  -- response <- Http.httpBS req
  -- let body = Http.getResponseBody response
  -- writeFileBS "/home/vodfsh/Downloads/hgit_refdiscovery.bin" body

  runConduitRes $
    C.sourceFile "/home/vodfsh/Downloads/hgit_refdiscovery.bin"
      -- Http.httpSource req getSrc
      .| pktLineDecoder
      .| infoRefsSmartS

data AckType = AckSimple deriving (Show, Eq)
data Ack = Ack {ackHash :: Hash, ackType :: AckType} deriving (Show, Eq)

-- returns either [Ack] or path to the tmp packfile file
gitUploadPackS :: (MonadIO m) => ConduitT ByteString Void m (Either [Ack] String)
gitUploadPackS = do
  print "everything:"
  everything <- foldC
  print $ BS.take 500 everything
  leftover everything

  (acks, nak, rest) <-
    pktLineDecoder .| do
      a <- takeWhileC ("ACK" `BS.isPrefixOf`) .| mapC parseAckSimple .| sinkList
      n <- takeWhileC (== "NAK") .| headC
      r <- sinkList
      return (a, n, r)

  print "acks:"
  mapM_ print acks

  print "nak:"
  print nak

  print "rest:"
  print rest

  packHeader <- takeCE 4 .| foldC
  case packHeader of
    "PACK" -> do
      leftover "PACK"
      packfile <- foldC
      return $ Right "packfile"
    "" -> do
      return $ Left acks
    _ -> throwErr "gitUploadPackS" "leftover data"
 where
  parseAckSimple bs = Ack (asciiToHash $ BS.drop 4 bs) AckSimple

gitFetch :: FetchOptions -> IO ()
gitFetch FetchOptions{..} = runWithFoundRepo $ do
  let url = normalizeGitUrl "https://github.com/LostInTheLogs/gleam_exercises" -- TODO:
  uploadPackReqEmpty <- hgitRequest $ "POST " <> url <> "/git-upload-pack"

  (capabilities, remoteRefs) <- refDiscovery url
  -- TODO: check if we have the remote refs already

  let wants = map head $ NE.group $ sort $ fst <$> remoteRefs -- TODO: handle HEAD instead of distinct
  uniqueRefs <- map head . NE.group . sort <$> collectRefs
  -- pending <- makeCmtQueue <$> mapM readCommit [asciiToHash ("d774d7c035dad1ba94aec0e999f451cfb3582bb9" :: Text)] -- more rounds
  pending <- makeCmtQueue <$> mapM readCommit [asciiToHash ("44b08a3abb38383ce4313b4fb511d1387de8394b" :: Text)] -- one round
  -- pending <- makeCmtQueue <$> mapM readCommit (fst <$> uniqueRefs) -- HEAD
  negotiate uploadPackReqEmpty wants pending [] 0

  pass
 where
  negotiate reqEmpty wants pending common sent = do
    print $ "negotation round " <> show sent
    let n = 10 -- TODO: 32
    (batch, newPending) <- popN pending n
    let haves = fst <$> batch
    let commits = Map.fromList batch

    let wantsBody = commandBuilder "want" wants
    let havesBody = commandBuilder "have" $ common ++ haves

    let getCachedParents hash = case Map.lookup hash commits of
          Just Commit{..} -> commitParents ++ concatMap getCachedParents commitParents
          Nothing -> []
    let getCachedLeaves hash = case Map.lookup hash commits of
          Just Commit{..} -> concatMap getCachedLeaves commitParents
          Nothing -> [hash]

    let givingUp = length haves < n
    let ending = if givingUp then pktLineB "done" else pktLineB ""
    let reqBody = B.toLazyByteString $ wantsBody <> pktLineB "" <> havesBody <> ending
    let req = Http.setRequestBodyLBS reqBody reqEmpty
    print reqBody

    res <-
      runConduitRes $
        Http.httpSource req getSrc
          -- .| foldC
          .| gitUploadPackS

    print ""
    print "res:"
    print res

    case res of
      Right file -> return file
      Left acks -> do
        let ackHashes = ackHash <$> acks
        let filteredAcks = flipfoldl' (\a -> let parents = getCachedParents a in filter (`notElem` parents)) ackHashes ackHashes
        let toRemove = concatMap getCachedLeaves filteredAcks
        let filteredPending = first (Q.filter (\i -> commitHash i `notElem` toRemove)) newPending
        negotiate reqEmpty wants filteredPending (common ++ filteredAcks) (sent + n)

  commandBuilder cmd = foldMap (\a -> pktLineB (cmd <> " " <> show a))

  popN :: CommitQueue -> Int -> WithRepository ([(Hash, Commit)], CommitQueue)
  popN = go []
   where
    go acc queue 0 = return (reverse acc, queue)
    go acc queue k = do
      popped <- cmtQueuePop queue
      case popped of
        (Nothing, _) -> return (reverse acc, queue)
        (Just cmt, nextQ) -> go ((commitHash cmt, cmt) : acc) nextQ (k - 1)
