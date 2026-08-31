{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module HGit.TransportUtils where

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
import HGit.Ref (collectRefs)
import HGit.Repository (WithRepository, runWithFoundRepo)
import HGit.Types (Hash (..), asciiHashFParser, asciiToHash, byteHashFParser, hashToAscii)
import HGit.Utils
import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types as HttpT
import Relude
import Relude.Extra (toFst)
import Text.Printf (printf)

type PktLineData = ByteString

normalizeGitUrl :: Text -> Text
normalizeGitUrl urlRaw = do
  let urlNoBS = dropSuffix "/" urlRaw
  if ".git" `T.isSuffixOf` urlNoBS then urlNoBS else urlNoBS <> ".git"

pktLineB :: PktLineData -> B.Builder
pktLineB "" = B.byteString "0000"
pktLineB content = do
  let len = printf "%04x" $ 5 + BS.length content
  B.string7 len <> B.byteString content <> B.char7 '\n'

commandBuilder :: (Foldable t) => PktLineData -> t PktLineData -> B.Builder
commandBuilder cmd = foldMap (\a -> pktLineB (cmd <> " " <> a))

commandBuilderWCaps :: PktLineData -> Capabilities -> NonEmpty PktLineData -> B.Builder
commandBuilderWCaps cmd caps args = do
  let firstArg :| restArgs = args
  let argWithCaps = firstArg <> " " <> BS.intercalate " " (toCapabilityStrings caps)
  commandBuilder cmd [argWithCaps] <> commandBuilder cmd restArgs

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

-- multi_ack thin-pack side-band side-band-64k ofs-delta shallow deepen-since
-- deepen-not deepen-relative no-progress include-tag multi_ack_detailed
-- allow-tip-sha1-in-want allow-reachable-sha1-in-want no-done
-- symref=HEAD:refs/heads/main filter object-format=sha1
-- agent=git/github-7cf87d205eb3-Linux print capabilities
data Capabilities = Capabilities {capMultiAck :: Bool, capMultiAckDetailed :: Bool, capAgent :: ByteString} deriving (Eq, Show)

data CapSpec = CapSpec
  { capName :: ByteString
  , capSet :: Either (Bool -> Capabilities -> Capabilities) (ByteString -> Capabilities -> Capabilities)
  , capGet :: Either (Capabilities -> Bool) (Capabilities -> ByteString)
  }

emptyCapabilities :: Capabilities
emptyCapabilities = Capabilities False False ""

capSpecs :: [CapSpec]
capSpecs =
  [ CapSpec "multi_ack" (Left $ \v c -> c{capMultiAck = v}) (Left capMultiAck)
  , CapSpec "multi_ack_detailed" (Left $ \v c -> c{capMultiAckDetailed = v}) (Left capMultiAckDetailed)
  , CapSpec "agent" (Right $ \v c -> c{capAgent = v}) (Right capAgent)
  ]
capMap :: HashMap ByteString CapSpec
capMap = Map.fromList $ toFst capName <$> capSpecs

fromCapabilityStrings :: [ByteString] -> Capabilities
fromCapabilityStrings = foldr applyCap emptyCapabilities
 where
  applyCap string caps = do
    let (k, v) = BS.drop 1 <$> BSC8.break (== '=') string

    case Map.lookup k capMap of
      Just CapSpec{capSet = Left capSet} ->
        capSet True caps
      Just CapSpec{capSet = Right capSet} ->
        capSet v caps
      Nothing -> caps

toCapabilityStrings :: Capabilities -> [ByteString]
toCapabilityStrings caps = filter (not . BS.null) $ capToStr <$> capSpecs
 where
  capToStr (CapSpec{capName = capName, capGet = Left capGet}) =
    if capGet caps then capName else ""
  capToStr (CapSpec{capName = capName, capGet = Right capGet}) =
    if not $ BS.null $ capGet caps then capName <> "=" <> capGet caps else ""

makeClientCaps :: Capabilities -> Capabilities
makeClientCaps serverCaps =
  emptyCapabilities
    { capAgent = "hgit"
    , capMultiAckDetailed = capMultiAckDetailed serverCaps
    , capMultiAck = capMultiAck serverCaps && not (capMultiAckDetailed serverCaps)
    }
