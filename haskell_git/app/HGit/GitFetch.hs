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
import qualified Data.HashMap.Lazy as Map
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.PQueue.Max as Q
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as FP
import HGit.Commit (Commit (..), CommitQueue, cmtQueuePop, commitHash, makeCmtQueue, readCommit)
import HGit.Config (readConfig)
import HGit.Object (readObj)
import HGit.Packfile (Pack (..), indexPack, readPack)
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, gitPath, packPath, runWithFoundRepo)
import HGit.TransportUtils
import HGit.Types (Hash (..), asciiHashFParser, asciiToHash, byteHashFParser, hashToAscii)
import HGit.Utils
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types as HttpT
import Relude
import Relude.Extra (toFst)
import System.FilePath
import qualified System.FilePattern as Glob
import System.IO (openTempFileWithDefaultPermissions)
import System.IO.MMap (Mode (ReadOnly), mmapFileByteString, mmapFilePtr, mmapWithFilePtr)
import Text.Printf (printf)
import UnliftIO hiding (atomically)
import UnliftIO.Directory (renameFile)
import qualified UnliftIO.Directory as Dir

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
      .| mapC (runFParserUnsafe refFP)
      .| sinkList

  return (fromCapabilityStrings $ BSC8.split ' ' capabilities, firstRef : restRefs)
 where
  parseAwait parser = runFParserUnsafe parser <$> awaitUnsafe
  refFP = do
    hash <- asciiHashFParser
    _spc <- $(FP.char ' ')
    name <- FP.takeRest
    return (hash, name)

refDiscovery :: (MonadUnliftIO m) => Text -> m (Capabilities, [(Hash, ByteString)])
refDiscovery url = do
  -- req <- hgitRequest $ "GET " <> url <> "/info/refs?service=git-upload-pack"
  -- response <- Http.httpBS req
  -- let body = Http.getResponseBody response
  -- writeFileBS "/home/vodfsh/Downloads/hgit_refdiscovery.bin" body -- TODO
  trace "TODO use req" $
    runConduitRes $
      C.sourceFile "/home/vodfsh/Downloads/hgit_refdiscovery.bin"
        -- Http.httpSource req getSrc
        .| pktLineDecoder
        .| infoRefsSmartS

data AckType = AckSimple | AckContinue | AckCommon | AckReady deriving (Show, Eq)
data Ack = Ack {ackHash :: Hash, ackType :: AckType} deriving (Show, Eq)

gitUploadPackS ::
  (MonadResource m) =>
  Capabilities ->
  TQueue (Maybe ByteString) ->
  TQueue (Maybe ByteString) ->
  ConduitT ByteString Void m (Maybe [Ack])
gitUploadPackS _caps packQ sideQ = bracketP pass (const cleanup) $ const $ do
  (acks, _nak, _rest) <-
    pktLineDecoder .| do
      a <- takeWhileC ("ACK" `BS.isPrefixOf`) .| mapC parseAck .| sinkList
      n <- takeWhileC (== "NAK") .| headC
      r <- sinkList
      return (a, n, r)

  packHeader <- takeCE 4 .| foldC
  case packHeader of
    "PACK" -> do
      leftover "PACK"

      awaitForever $ \x -> liftIO $ atomically $ writeTQueue packQ (Just x)
      return Nothing
    "" -> do
      return $ Just acks
    _ -> do
      throwErr "gitUploadPackS" "leftover data"
 where
  cleanup = do
    closeQueue packQ
    closeQueue sideQ
  parseAck bs = do
    let (hash, ackType) = BS.drop 1 <$> BS.splitAt 40 (BS.drop 4 bs)
    Ack (asciiToHash hash) (parseAckType ackType)
  parseAckType "continue" = AckContinue
  parseAckType "common" = AckCommon
  parseAckType "ready" = AckReady
  parseAckType "" = AckSimple
  parseAckType _ = throwErr "parseAckType" "Unknown type"

negotiate :: FilePath -> Handle -> Capabilities -> Http.Request -> NonEmpty Hash -> CommitQueue -> [Hash] -> Int -> WithRepository ()
negotiate path h caps reqEmpty wants oldPending common sent = do
  let n = if sent > 32 then 64 else 32
  (batch, pending) <- popN oldPending n
  let haves = fst <$> batch

  let wantsBody = commandBuilderWCaps "want" caps $ hashToAscii <$> wants
  let havesBody = commandBuilder "have" $ hashToAscii <$> common ++ haves

  let givingUp = length haves < n || (sent > 256 && not (null common))

  let reqBody =
        B.toLazyByteString $
          wantsBody
            <> pktLineB ""
            <> havesBody
            <> if givingUp then pktLineB "done" else pktLineB ""

  let req = Http.setRequestBodyLBS reqBody reqEmpty

  packfileQ <- newTQueueIO
  sideQ <- newTQueueIO

  let source =
        Http.httpSource req getSrc
          .| gitUploadPackS caps packfileQ sideQ

  (maybeAcks, (), ()) <-
    runConcurrently $
      (,,)
        <$> Concurrently (runConduitRes source)
        <*> Concurrently (runConduit $ sourceCloseableQueue packfileQ .| sinkHandle h)
        <*> Concurrently (runConduit $ sourceCloseableQueue sideQ .| mapM_C (\x -> print $ "Thread side: " <> x))

  let commits = Map.fromList batch
  let getCachedParents hash = case Map.lookup hash commits of
        Just Commit{..} -> commitParents ++ concatMap getCachedParents commitParents
        Nothing -> []
  let getCachedLeaves hash = case Map.lookup hash commits of
        Just Commit{..} -> concatMap getCachedLeaves commitParents
        Nothing -> [hash]

  case maybeAcks of
    Nothing -> pass
    Just acks -> do
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

      negotiate path h caps reqEmpty wants filteredPending (common ++ filteredAcks) (sent + n)
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

-- | Matches refs against refspecs, returns the matches refs and actions writing them to disk
matchUpdateRefs :: [Text] -> [(Hash, ByteString)] -> ([(Hash, ByteString)], [WithRepository ()])
matchUpdateRefs specs refs = do
  let matches = Glob.matchMany (parseSpec <$> specs) (parseRef <$> refs)

  unzip $ workMatch <$> matches
 where
  parseRef (hash, ref) = ((hash, ref), decodeUtf8 ref)

  parseSpec :: Text -> ((Bool, Text), String)
  parseSpec spec = do
    let (rest, to) = T.drop 1 <$> T.break (== ':') spec
    let (overwrite, from) = case T.stripPrefix "+" rest of
          Just x -> (True, x)
          Nothing -> (False, rest)

    ((overwrite, to), toString from)

  workMatch ((overwrite, outglob), (hash, ref), match) = do
    let outGitPath = case viaNonEmpty head match of
          Just m -> T.replace "*" (toText m) outglob
          Nothing -> outglob

    let write = do
          path <- gitPath [toString outGitPath]
          Dir.createDirectoryIfMissing True $ takeDirectory path
          exists <- Dir.doesFileExist path
          unless (exists && not overwrite) $ do
            writeFileBS path $ hashToAscii hash <> "\n"

    ((hash, ref), write)

gitFetch :: FetchOptions -> IO ()
gitFetch FetchOptions{} = runWithFoundRepo $ do
  config <- readConfig
  let remoteConfig = config Map.! ("remote", "origin")
  let url = normalizeGitUrl $ last $ remoteConfig Map.! "url"
  let fetchSpecs = remoteConfig Map.! "fetch"

  (serverCaps, unfilteredRemoteRefs) <- refDiscovery url

  let (matchingRefs, updateRefsActions) = matchUpdateRefs (toList fetchSpecs) unfilteredRemoteRefs

  -- TODO: after unpacking the packfile, WIPE CACHE, and write all new reachable tags to /refs/tags
  -- TODO: second POST with annotated tags
  -- TODO: sideband

  let wants = NE.fromList $ map head $ NE.group $ sort $ fst <$> matchingRefs
  uniqueRefs <- map head . NE.group . sort <$> collectRefs
  pending <- makeCmtQueue <$> mapM readCommit (fst <$> uniqueRefs)
  packPth <- packPath []

  withTempFile packPth "fetch-packfile" $ \path h -> do
    putTextLn "Fetching..."
    let clientCaps = makeClientCapabilities serverCaps
    uploadPackReqEmpty <- hgitRequest $ "POST " <> url <> "/git-upload-pack"
    negotiate path h clientCaps uploadPackReqEmpty wants pending [] 0

    hClose h
    idxFile <- mmapWithBytestring path $ \raw -> do
      let Pack{pckCount = count} = readPack raw
      -- TODO: if less than 100 objects, unpack to loose
      if count == 0
        then do
          putTextLn "Nothing to fetch."
          return Nothing
        else do
          putTextLn "Indexing..."
          Just <$> indexPack raw readObj

    case idxFile of
      Nothing -> pass
      Just idxPath -> do
        let packFile = replaceExtension idxPath "pack"
        renameFile path packFile
        print packFile

  sequenceA_ updateRefsActions
