{-# LANGUAGE ForeignFunctionInterface #-}

module HGit.ZLib (decompressExact) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Streaming.Zlib
import Data.Streaming.Zlib.Lowlevel
import Foreign
import Foreign.C (CChar)
import Foreign.C.Types (CInt)
import GHC.ForeignPtr
import HGit.Utils (throwErr)
import Relude
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (bracket)

-- modified functions from
-- https://hackage-content.haskell.org/package/streaming-commons-0.2.3.1/docs/src/Data.Streaming.Zlib.html

foreign import ccall "dynamic"
  mkFreeZStream :: FunPtr (ZStream' -> IO ()) -> (ZStream' -> IO ())

withInflate :: (ZStream' -> IO c) -> IO c
withInflate = bracket allocZStream freeZStream
 where
  allocZStream = do
    zstr <- zstreamNew
    inflateInit2 zstr defaultWindowBits
    pure zstr

  freeZStream zstr = do
    mkFreeZStream c_free_z_stream_inflate zstr

decompressExact :: ByteString -> Int -> ByteString
decompressExact bs outSize = unsafePerformIO $ withInflate $ \zstrPtr -> do
  outputFPtr <- mallocPlainForeignPtrBytes outSize
  withForeignPtr outputFPtr $ \outputPtr ->
    c_set_avail_out zstrPtr outputPtr $ fromIntegral outSize

  unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    c_set_avail_in zstrPtr cstr $ fromIntegral len

  res <- c_call_inflate_noflush zstrPtr

  when (res < 0) $ throwErr "decompressExact" $ "decompression failed, errno: " <> show res

  availOut <- c_get_avail_out zstrPtr

  when (availOut /= 0) $ throwErr "decompressExact" "wrong output size"

  return $ BSI.fromForeignPtr0 (castForeignPtr outputFPtr) outSize

{- | Feed the given 'S.ByteString' to the inflater. Return a 'Popper',
an IO action that returns the decompressed data a chunk at a time.
The 'Popper' must be called to exhaustion before using the 'Inflate'
object again.

Note that this function automatically buffers the output to
'defaultChunkSize', and therefore you won't get any data from the popper
until that much decompressed data is available. After you have fed all of
the compressed data to this function, you can extract your final chunk of
decompressed data using 'finishInflate'.
-}

-- drain ::
--   ForeignPtr CChar ->
--   ForeignPtr ZStreamStruct ->
--   (ZStream' -> IO CInt) ->
--   Bool ->
--   Popper
-- drain fbuff fzstr func isFinish = withForeignPtr fzstr $ \zstr -> do
--   res <- func zstr
--   if res < 0 && res /= zBufError
--     then return $ PRError $ ZlibException $ fromIntegral res
--     else do
--       avail <- c_get_avail_out zstr
--       let size = defaultChunkSize - fromIntegral avail
--           toOutput = avail == 0 || (isFinish && size /= 0)
--       if toOutput
--         then withForeignPtr fbuff $ \buff -> do
--           bs <- S.packCStringLen (buff, size)
--           c_set_avail_out zstr buff $
--             fromIntegral defaultChunkSize
--           return $ PRNext bs
--         else return PRDone

-- feedInflate (Inflate (fzstr, fbuff) lastBS complete inflateDictionary) bs = do
--   -- Write the BS to lastBS for use by getUnusedInflate. This is
--   -- theoretically unnecessary, since we could just grab the pointer from the
--   -- fzstr when needed. However, in that case, we wouldn't be holding onto a
--   -- reference to the ForeignPtr, so the GC may decide to collect the
--   -- ByteString in the interim.
--   writeIORef lastBS bs

--   withForeignPtr fzstr $ \zstr ->
--     unsafeUseAsCStringLen bs $ \(cstr, len) ->
--       c_set_avail_in zstr cstr $ fromIntegral len
--   return $ drain fbuff fzstr inflate False
--  where
--   inflate zstr = do
--     res <- c_call_inflate_noflush zstr
--     res2 <-
--       if (res == zNeedDict)
--         then
--           maybe
--             (return zNeedDict)
--             ( \dict ->
--                 ( unsafeUseAsCStringLen dict $ \(cstr, len) -> do
--                     c_call_inflate_set_dictionary zstr cstr $ fromIntegral len
--                     c_call_inflate_noflush zstr
--                 )
--             )
--             inflateDictionary
--         else return res
--     when (res2 == zStreamEnd) (writeIORef complete True)
--     return res2

{- | An IO action that returns the next chunk of data, returning 'PRDone' when
there is no more data to be popped.
-}

-- type Popper = IO PopperRes

-- data PopperRes
--   = PRDone
--   | PRNext !S.ByteString
--   | PRError !ZlibException
--   deriving (Show, Typeable)

{- | As explained in 'feedInflate', inflation buffers your decompressed
data. After you call 'feedInflate' with your last chunk of compressed
data, you will likely have some data still sitting in the buffer. This
function will return it to you.
-}

-- finishInflate :: Inflate -> IO S.ByteString
-- finishInflate (Inflate (fzstr, fbuff) _ _ _) =
--   withForeignPtr fzstr $ \zstr ->
--     withForeignPtr fbuff $ \buff -> do
--       avail <- c_get_avail_out zstr
--       let size = defaultChunkSize - fromIntegral avail
--       bs <- S.packCStringLen (buff, size)
--       c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
--       return bs

-- getUnusedInflate :: Inflate -> IO S.ByteString
-- getUnusedInflate (Inflate (fzstr, _) ref _ _) = do
--   bs <- readIORef ref
--   len <- withForeignPtr fzstr c_get_avail_in
--   return $ S.drop (S.length bs - fromIntegral len) bs

--
--
--

-- import Codec.Compression.Zlib.Stream
-- import qualified Data.ByteString as S
-- import qualified Data.ByteString.Unsafe as SU
-- import Foreign.ForeignPtr (withForeignPtr)
-- import Foreign.Ptr (castPtr)
-- import System.IO.Unsafe (unsafePerformIO)

-- -- | Decompress a Git object payload directly into a single pre-allocated ByteString.
-- -- Inputs:
-- --   - expectedSize: Decompressed size from the Git pack object header
-- --   - inputSlice:   ByteString slice starting at payload offset, extending to mmap end
-- --
-- -- Returns:
-- --   - (Decompressed ByteString, Exact Compressed Bytes Consumed)
-- decompressExact
--   :: Int          -- ^ Expected decompressed byte length
--   -> S.ByteString -- ^ Source input slice from mmap
--   -> Either String (S.ByteString, Int)
-- decompressExact expectedSize inputSlice = unsafePerformIO $ do
--   -- 1. Initialize zlib stream for raw/zlib format
--   defStream <- initInflate defaultWindowBits

--   -- 2. Allocate the exact output buffer upfront without zero-filling
--   outBs <- SU.unsafeCreate expectedSize $ \outPtr -> do
--     -- Set zlib's next_out directly to our destination buffer!
--     setOutBuf defStream (castPtr outPtr) expectedSize

--     -- 3. Pass the input mmap pointer directly to zlib's next_in
--     SU.unsafeUseAsCStringLen inputSlice $ \(inPtr, inLen) -> do
--       setInBuf defStream inPtr inLen

--       -- 4. Inflate in a single pass (Z_FINISH mode)
--       _ <- inflate defStream Finish
--       pure ()

--   -- 5. Inspect total_in to see how many compressed bytes zlib consumed
--   consumed <- getTotalIn defStream

--   -- Verify all expected bytes were produced
--   availOut <- getAvailOut defStream
--   if availOut == 0
--     then pure $ Right (outBs, fromIntegral consumed)
--     else pure $ Left $ "Zlib stream ended early. Expected " ++ show expectedSize ++ " bytes, got " ++ show (expectedSize - availOut)

--
--
--
--

-- -- | Read a loose object from raw zlib-compressed file bytes.
-- readLooseObject :: S.ByteString -> Either String LooseObject
-- readLooseObject compressedFile = unsafePerformIO $ do
--   defStream <- initInflate defaultWindowBits

--   -- Step 1: Inflate a 512-byte header scratch buffer
--   scratchBs <- SU.unsafeCreate 512 $ \scratchPtr -> do
--     setOutBuf defStream (castPtr scratchPtr) 512
--     SU.unsafeUseAsCStringLen compressedFile $ \(inPtr, inLen) -> do
--       setInBuf defStream inPtr inLen
--       _ <- inflate defStream Sync -- Inflate just enough to fill or start
--       pure ()

--   -- Step 2: Parse ASCII header "<type> <size>\0"
--   case S.elemIndex 0 scratchBs of
--     Nothing -> pure $ Left "Loose object header exceeded 512 bytes"
--     Just nulIdx -> do
--       let (hdr, rest) = S.splitAt nulIdx scratchBs
--           payloadAlreadyDecoded = S.drop 1 rest -- strip the NUL byte
--           (typeBs, sizeBs) = S.break (== 32) hdr -- split on space (ASCII 32)
--           objSize = read (map (chr . fromIntegral) . S.unpack $ S.drop 1 sizeBs)
--           copiedLen = S.length payloadAlreadyDecoded

--       -- Step 3: Allocate the EXACT remaining payload buffer
--       payloadBs <- SU.unsafeCreate objSize $ \outPtr -> do
--         -- Copy over bytes already inflated into the scratch buffer
--         SU.unsafeUseAsCStringLen payloadAlreadyDecoded $ \(srcPtr, len) ->
--           copyBytes outPtr (castPtr srcPtr) len

--         -- Point zlib directly into the remaining space of our new buffer!
--         let destPtr = outPtr `plusPtr` copiedLen
--             remSize = objSize - copiedLen

--         setOutBuf defStream (castPtr destPtr) remSize
--         _ <- inflate defStream Finish
--         pure ()

--       pure $ Right $ LooseObject typeBs objSize payloadBs
