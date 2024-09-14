{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.MemPack.Buffer
-- Copyright   : (c) Alexey Kuleshevich 2024
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack.Buffer where

import Data.Array.Byte
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.ByteString.Internal as BS
import GHC.Exts
import GHC.IO
import GHC.ForeignPtr

-- | Immutable memory buffer
class Buffer b where
  bufferByteCount :: b -> Int

  buffer :: b -> (ByteArray# -> a) -> (Addr# -> a) -> a

instance Buffer ByteArray where
  bufferByteCount (ByteArray ba#) = I# (sizeofByteArray# ba#)
  {-# INLINE bufferByteCount #-}

  buffer (ByteArray ba#) f _ = f ba#
  {-# INLINE buffer #-}

instance Buffer SBS.ShortByteString where
  bufferByteCount = SBS.length
  {-# INLINE bufferByteCount #-}

  buffer (SBS.SBS ba#) f _ = f ba#
  {-# INLINE buffer #-}

instance Buffer BS.ByteString where
  bufferByteCount = BS.length
  {-# INLINE bufferByteCount #-}

  buffer bs _ f =
    BS.accursedUnutterablePerformIO $ withPtrByteString bs $ \(Ptr addr#) -> pure (f addr#)
  {-# INLINE buffer #-}

withPtrByteString :: BS.ByteString -> (Ptr a -> IO b) -> IO b
#if MIN_VERSION_bytestring(0,11,0)
withPtrByteString (BS.BS (ForeignPtr addr# ptrContents) _) f = do
#else
withPtrByteString (BS.PS (ForeignPtr addr0# ptrContents) (I# offset#) _) f = do
  let !addr# = addr0# `plusAddr#` offset#
#endif
  !r <- f (Ptr addr#)
  IO $ \s# -> (# touch# ptrContents s#, () #)
  pure r
{-# INLINE withPtrByteString #-}

-- bufUnsafeIndex :: b -> Int -> Word8

-- bufUnsafeCopySliceInto :: MutableByteArray s -> b -> Int -> Int -> ST s ()

--bufToByteArray :: b -> ByteArray

-- bufToByteString :: b -> ByteString

-- bufSliceToByteArray :: b -> Int -> Int -> ByteArray

-- bufSliceToByteString :: b -> Int -> Int -> ByteString


pinnedByteArrayToByteString :: ByteArray -> BS.ByteString
pinnedByteArrayToByteString (ByteArray ba#) =
  BS.PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}


byteArrayToShortByteString :: ByteArray -> SBS.ShortByteString
byteArrayToShortByteString (ByteArray ba#) = SBS.SBS ba#
{-# INLINE byteArrayToShortByteString #-}
