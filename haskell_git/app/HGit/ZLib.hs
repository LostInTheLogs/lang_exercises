{-# LANGUAGE ForeignFunctionInterface #-}

module HGit.ZLib (decompressExact, decompressExactTwoPass) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
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

zBufError :: CInt
zBufError = -5

zOk :: CInt
zOk = 0

zStreamEnd :: CInt
zStreamEnd = 1

withInflate :: (ZStream' -> IO c) -> IO c
withInflate = bracket allocZStream freeZStream
 where
  allocZStream = do
    zstr <- zstreamNew
    inflateInit2 zstr defaultWindowBits
    pure zstr

  freeZStream zstr = do
    mkFreeZStream c_free_z_stream_inflate zstr

decompressExact :: ByteString -> Int -> (ByteString, ByteString)
decompressExact bs outSize = unsafePerformIO $ withInflate $ \zstrPtr -> do
  outputFPtr <- mallocPlainForeignPtrBytes outSize
  unsafeWithForeignPtr outputFPtr $ \outputPtr ->
    c_set_avail_out zstrPtr outputPtr $ fromIntegral outSize

  unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    c_set_avail_in zstrPtr cstr $ fromIntegral len

  res <- c_call_inflate_noflush zstrPtr

  when (res /= zStreamEnd) $ throwErr "decompressExact" $ "decompression failed, errno: " <> show res

  availOut <- c_get_avail_out zstrPtr

  when (availOut /= 0) $ throwErr "decompressExact" "wrong output size"

  leftover <- flip BS.takeEnd bs . fromIntegral <$> c_get_avail_in zstrPtr

  let output = BSI.fromForeignPtr0 (castForeignPtr outputFPtr) outSize

  return (output, leftover)

decompressExactTwoPass :: ByteString -> (ByteString -> Int) -> (ByteString, ByteString)
decompressExactTwoPass bs lenReader = unsafePerformIO $ withInflate $ \zstrPtr -> do
  let scratchSize = 64
  scratchFPtr <- mallocPlainForeignPtrBytes scratchSize
  unsafeWithForeignPtr scratchFPtr $ \ptr ->
    c_set_avail_out zstrPtr ptr $ fromIntegral scratchSize

  unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    c_set_avail_in zstrPtr cstr $ fromIntegral len

  res <- c_call_inflate_noflush zstrPtr

  when (res /= zOk && res /= zStreamEnd) $ throwErr "decompressExactTwoPass" $ "decompression failed, errno: " <> show res

  availOut <- fromIntegral <$> c_get_avail_out zstrPtr

  let scratchBsLen = scratchSize - availOut
  -- do not return
  let scratchBS = BSI.fromForeignPtr0 (castForeignPtr scratchFPtr) scratchBsLen

  if res == zStreamEnd
    -- finished already
    then do
      leftover <- flip BS.takeEnd bs . fromIntegral <$> c_get_avail_in zstrPtr
      return (BS.copy scratchBS, leftover)
    -- need more output space
    else do
      let outSize = lenReader scratchBS

      outputFPtr <- mallocPlainForeignPtrBytes outSize
      unsafeWithForeignPtr scratchFPtr $ \scratchPtr ->
        unsafeWithForeignPtr outputFPtr $ \outputPtr -> do
          copyBytes outputPtr scratchPtr scratchBsLen
          c_set_avail_out zstrPtr (plusPtr outputPtr scratchBsLen) $ fromIntegral (outSize - scratchBsLen)

      res2 <- c_call_inflate_noflush zstrPtr

      when (res2 /= zStreamEnd) $ throwErr "decompressExactTwoPass" $ "decompression 2 failed, errno: " <> show res2

      availOut2 <- c_get_avail_out zstrPtr
      when (availOut2 /= 0) $ throwErr "decompressExactTwoPass" "wrong output size"

      let output = BSI.fromForeignPtr0 (castForeignPtr outputFPtr) outSize
      leftover <- flip BS.takeEnd bs . fromIntegral <$> c_get_avail_in zstrPtr

      return (output, leftover)
