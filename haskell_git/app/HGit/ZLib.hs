{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HGit.ZLib (decompressExact, decompressExactTwoPass, CRC32, crc32, crc32Update) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSLI
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Streaming.Zlib
import Data.Streaming.Zlib.Lowlevel
import Foreign
import Foreign.C
import GHC.ForeignPtr
import HGit.Utils (throwErr)
import Relude
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (bracket)

-- TODO: lazy bytestrings with unsafeInterleaveIO or a conduit
-- input can be strict, because it's mmap'ed

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

-- CRC code taken from <https://github.com/TeofilC/digest/blob/0fe4c403b9a90b60ed937af685dbc9a98e3af39a/Data/Digest/CRC32.hsc>
-- with license
{- Copyright (c) 2008-2009, Eugene Kirpichov
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE. -}

-- | The class of values for which CRC32 may be computed
class CRC32 a where
  -- | Compute CRC32 checksum
  crc32 :: a -> Word32
  crc32 = crc32Update 0

  -- | Given the CRC32 checksum of a string, compute CRC32 of its
  -- concatenation with another string (t.i., incrementally update
  -- the CRC32 hash value)
  crc32Update :: Word32 -> a -> Word32

instance CRC32 ByteString where
  crc32Update = crc32_s_update

instance CRC32 BSL.ByteString where
  crc32Update = crc32_l_update

instance CRC32 [Word8] where
  crc32Update n = crc32Update n . BSL.pack

crc32_s_update :: Word32 -> ByteString -> Word32
crc32_s_update seed str
  | BS.null str = seed
  | otherwise =
      unsafePerformIO $
        unsafeUseAsCStringLen str $
          \(buf, len) ->
            fromIntegral <$> crc32_c (fromIntegral seed) (castPtr buf) (fromIntegral len)

crc32_l_update :: Word32 -> BSL.ByteString -> Word32
crc32_l_update = BSLI.foldlChunks crc32_s_update

foreign import ccall unsafe "zlib.h crc32"
  crc32_c ::
    CULong ->
    Ptr Word8 ->
    CUInt ->
    IO CULong
