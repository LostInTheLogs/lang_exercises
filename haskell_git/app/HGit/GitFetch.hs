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
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.PQueue.Max as Q
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as FP
import HGit.Commit (Commit (..), CommitQueue, cmtQueuePop, commitHash, makeCmtQueue, readCommit)
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, runWithFoundRepo)
import HGit.TransportUtils
import HGit.Types (Hash (..), asciiHashFParser, asciiToHash, byteHashFParser, hashToAscii)
import HGit.Utils
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types as HttpT
import Relude
import Relude.Extra (toFst)
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

-- https://git-scm.com/docs/http-protocol
-- https://git-scm.com/docs/pack-protocol
-- https://git-scm.com/docs/protocol-capabilities
-- https://git-scm.com/docs/gitprotocol-pack

data FetchOptions = FetchOptions {}

infoRefsSmartS :: (Monad m) => ConduitT PktLineData Void m (Capabilities, [(Hash, ByteString)])
infoRefsSmartS = do
  _serviceLine <- awaitUnsafe

  _ <- awaitExactly ""

  (firstRef, capabilities) <- parseAwait (pktLineWExtraFP refFP FP.takeRest)

  restRefs <-
    takeWhileC (not . BS.null)
      .| foldMapC (List.singleton . runFParserUnsafe refFP)

  return (fromCapabilityStrings $ BSC8.split ' ' capabilities, firstRef : restRefs)
 where
  parseAwait parser = runFParserUnsafe parser <$> awaitUnsafe
  refFP = do
    hash <- asciiHashFParser
    _spc <- $(FP.char ' ')
    name <- FP.takeRest
    return (hash, name)

refDiscovery :: (MonadUnliftIO m) => p -> m (Capabilities, [(Hash, ByteString)])
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

data AckType = AckSimple | AckContinue | AckCommon | AckReady deriving (Show, Eq)
data Ack = Ack {ackHash :: Hash, ackType :: AckType} deriving (Show, Eq)

-- returns either [Ack] or path to the tmp packfile file
gitUploadPackS :: (MonadIO m) => Capabilities -> ConduitT ByteString Void m (Either [Ack] String)
gitUploadPackS caps = do
  print "everything:"
  everything <- foldC
  print $ BS.take 900 everything
  leftover everything

  (acks, nak, rest) <-
    pktLineDecoder .| do
      a <- takeWhileC ("ACK" `BS.isPrefixOf`) .| mapC parseAck .| sinkList
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
  parseAck bs = do
    let (hash, ackType) = BS.drop 1 <$> BS.splitAt 40 (BS.drop 4 bs)
    Ack (asciiToHash hash) (parseAckType ackType)
  parseAckType "continue" = AckContinue
  parseAckType "common" = AckCommon
  parseAckType "ready" = AckReady
  parseAckType "" = AckSimple
  parseAckType _ = throwErr "parseAckType" "Unknown type"

negotiate :: Capabilities -> Http.Request -> NonEmpty Hash -> CommitQueue -> [Hash] -> Int -> WithRepository String
negotiate caps reqEmpty wants oldPending common sent = do
  putBSLn $ "\n\n\tNEGOTATION ROUND " <> show sent
  let n = 10 -- TODO: if sent > 32 then 64 else 32
  (batch, pending) <- popN oldPending n
  let haves = fst <$> batch

  let wantsBody = commandBuilderWCaps "want" caps $ hashToAscii <$> wants
  let havesBody = commandBuilder "have" $ hashToAscii <$> common ++ haves

  let givingUp = length haves < n || (sent > 256 && not (null common))
  let ending = if givingUp then pktLineB "done" else pktLineB ""

  let reqBody = B.toLazyByteString $ wantsBody <> pktLineB "" <> havesBody <> ending
  print reqBody
  let req = Http.setRequestBodyLBS reqBody reqEmpty

  res <-
    runConduitRes $
      Http.httpSource req getSrc
        -- .| foldC
        .| gitUploadPackS caps

  print ""
  print "res:"
  print res

  let commits = Map.fromList batch
  let getCachedParents hash = case Map.lookup hash commits of
        Just Commit{..} -> commitParents ++ concatMap getCachedParents commitParents
        Nothing -> []
  let getCachedLeaves hash = case Map.lookup hash commits of
        Just Commit{..} -> concatMap getCachedLeaves commitParents
        Nothing -> [hash]

  case res of
    Right file -> return file
    Left acks -> do
      let ackHashes = ackHash <$> acks
      let ready = find (\(Ack _ ackType) -> ackType == AckReady) acks

      let filteredAcks = flipfoldl' (\a -> let parents = getCachedParents a in filter (`notElem` parents)) ackHashes ackHashes

      let filteredPending
            | null acks = pending
            | capMultiAckDetailed caps && isJust ready = (Q.empty, Set.empty)
            | capMultiAck caps || capMultiAckDetailed caps =
                let toRemove = concatMap getCachedLeaves filteredAcks
                 in first (Q.filter (\i -> commitHash i `notElem` toRemove)) pending
            | otherwise = (Q.empty, Set.empty)

      negotiate caps reqEmpty wants filteredPending (common ++ filteredAcks) (sent + n)
 where
  popN :: CommitQueue -> Int -> WithRepository ([(Hash, Commit)], CommitQueue)
  popN = go []
   where
    go acc queue 0 = return (reverse acc, queue)
    go acc queue k = do
      popped <- cmtQueuePop queue
      case popped of
        (Nothing, _) -> return (reverse acc, queue)
        (Just cmt, nextQ) -> go ((commitHash cmt, cmt) : acc) nextQ (k - 1)

gitFetch :: FetchOptions -> IO ()
gitFetch FetchOptions{..} = runWithFoundRepo $ do
  let url = normalizeGitUrl "https://github.com/LostInTheLogs/gleam_exercises" -- TODO:
  uploadPackReqEmpty <- hgitRequest $ "POST " <> url <> "/git-upload-pack"

  (serverCaps, remoteRefs) <- refDiscovery url
  let clientCaps = makeClientCaps serverCaps

  -- TODO: check if we have the remote refs already

  let wants = NE.fromList $ map head $ NE.group $ sort $ fst <$> remoteRefs -- TODO: handle HEAD instead of distinct
  uniqueRefs <- map head . NE.group . sort <$> collectRefs
  -- pending <- makeCmtQueue <$> mapM readCommit [asciiToHash ("d774d7c035dad1ba94aec0e999f451cfb3582bb9" :: Text)] -- more rounds
  pending <- makeCmtQueue <$> mapM readCommit [asciiToHash ("44b08a3abb38383ce4313b4fb511d1387de8394b" :: Text)] -- one round
  -- pending <- makeCmtQueue <$> mapM readCommit (fst <$> uniqueRefs) -- HEAD
  negotiate clientCaps uploadPackReqEmpty wants pending [] 0

  pass
