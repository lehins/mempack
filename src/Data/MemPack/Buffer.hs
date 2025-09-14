{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.MemPack.Buffer
-- Copyright   : (c) Alexey Kuleshevich 2024-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack.Buffer (
  Buffer (..),
  newMutableByteArray,
  freezeMutableByteArray,
  withPtrByteStringST,
  withAddrByteStringST,
  withForeignPtrST,
  pinnedByteArrayToByteString,
  pinnedByteArrayToForeignPtr,
  byteArrayToShortByteString,
  byteArrayFromShortByteString,
)
where

import Data.Array.Byte
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Primitive.PrimArray (PrimArray (..))
import Data.Word (Word8)
import GHC.Exts
import GHC.ForeignPtr
import GHC.ST
#if !MIN_VERSION_primitive(0,8,0)
import qualified Data.Primitive.ByteArray as Prim (ByteArray(..))
#endif
import qualified Data.Vector.Primitive as VP (Vector (..))
import qualified Data.Vector.Storable as VS (
  Vector,
  length,
  unsafeFromForeignPtr0,
  unsafeToForeignPtr0,
 )

-- | Immutable memory buffer
class Buffer b where
  -- | Number of accessible bytes in the buffer.
  bufferByteCount :: b -> Int

  -- | Use one of the two suppplied functions to access memory of the buffer:
  buffer ::
    -- | A type that contains the actual buffer that will be accessed
    b ->
    -- | In case when a buffer is backed by a `ByteArray#` it will be accessed as such with an
    -- offset from the beginning of the ByteArray
    (ByteArray# -> Int# -> a) ->
    -- | In case when a buffer is backed by a pointer or a pinned `ByteArray#` it can be accessed as
    -- an `Addr#`. No offset is necessary here, since same affect can be achieved with pointer
    -- arithmetic.
    (Addr# -> a) ->
    a

  mkBuffer :: ByteArray# -> b

  bufferHasToBePinned :: Bool

instance Buffer ByteArray where
  bufferByteCount (ByteArray ba#) = I# (sizeofByteArray# ba#)
  {-# INLINE bufferByteCount #-}
  buffer (ByteArray ba#) f _ = f ba# 0#
  {-# INLINE buffer #-}
  mkBuffer ba# = ByteArray ba#
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = False

#if !MIN_VERSION_primitive(0,8,0)
instance Buffer Prim.ByteArray where
  bufferByteCount (Prim.ByteArray ba#) = I# (sizeofByteArray# ba#)
  {-# INLINE bufferByteCount #-}
  buffer (Prim.ByteArray ba#) f _ = f ba# 0#
  {-# INLINE buffer #-}
  mkBuffer ba# = Prim.ByteArray ba#
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = False
#endif

instance Buffer SBS.ShortByteString where
  bufferByteCount = SBS.length
  {-# INLINE bufferByteCount #-}
  buffer (SBS.SBS ba#) f _ = f ba# 0#
  {-# INLINE buffer #-}
  mkBuffer ba# = SBS.SBS ba#
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = False

instance Buffer BS.ByteString where
  bufferByteCount = BS.length
  {-# INLINE bufferByteCount #-}
  buffer bs _f g =
    runST $ withAddrByteStringST bs $ \addr# -> pure (g addr#)
  {-# INLINE buffer #-}
  mkBuffer ba# = pinnedByteArrayToByteString (ByteArray ba#)
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = True

instance Buffer (PrimArray Word8) where
  bufferByteCount (PrimArray ba#) = I# (sizeofByteArray# ba#)
  {-# INLINE bufferByteCount #-}
  buffer (PrimArray ba#) f _ = f ba# 0#
  {-# INLINE buffer #-}
  mkBuffer ba# = PrimArray ba#
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = False

instance Buffer (VP.Vector Word8) where
  bufferByteCount (VP.Vector _ len _) = len
  {-# INLINE bufferByteCount #-}
  buffer (VP.Vector (I# off#) _ ba) f = buffer ba (\ba# _ -> f ba# off#)
  {-# INLINE buffer #-}
  mkBuffer ba# =
    let ba = mkBuffer ba#
     in VP.Vector 0 (bufferByteCount ba) ba
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = False

instance Buffer (VS.Vector Word8) where
  bufferByteCount = VS.length
  {-# INLINE bufferByteCount #-}
  buffer v _f g =
    runST $ withForeignPtrST (fst $ VS.unsafeToForeignPtr0 v) $ \addr# -> pure (g addr#)
  {-# INLINE buffer #-}
  mkBuffer ba# =
    VS.unsafeFromForeignPtr0 (pinnedByteArrayToForeignPtr ba#) (I# (sizeofByteArray# ba#))
  {-# INLINE mkBuffer #-}
  bufferHasToBePinned = True

-- | Allocate a new uninitialized `MutableByteArray`.
--
-- * __Warning__ - Memory allocated might contain random garbage and must be fully overwritten.
--
-- __⚠__ - Violation of the above rule could lead non-determinism and breakage of referential
-- transparency.
--
-- @since 0.1.0
newMutableByteArray ::
  -- | Should the mutable array be allocated as pinned or not
  Bool ->
  -- | Size of the mutable array in number of bytes.
  Int ->
  ST s (MutableByteArray s)
newMutableByteArray isPinned (I# len#) =
  ST $ \s# -> case (if isPinned then newPinnedByteArray# else newByteArray#) len# s# of
    (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newMutableByteArray #-}

-- | /O(1)/ - Cast a `MutableByteArray` to an immutable `ByteArray` without copy.
--
-- * __Warning__ - Source mutable array must not be mutated, after this action.
--
-- __⚠__ - Violation of the above rule could potentially lead to corrupt memory and segfaults.
--
-- @since 0.1.0
freezeMutableByteArray :: MutableByteArray d -> ST d ByteArray
freezeMutableByteArray (MutableByteArray mba#) =
  ST $ \s# -> case unsafeFreezeByteArray# mba# s# of
    (# s'#, ba# #) -> (# s'#, ByteArray ba# #)

-- | Run ST action on the underlying `Ptr` that points to the beginning of the `ByteString`
-- buffer. It is ok to use ByteString withing ST, as long as underlying pointer is never mutated or
-- returned from the supplied action.
--
-- * __Warning__ - It is important for the supplied action to not produce bottom, i.e. runtime
-- exceptions or infinite loops are not allowed within its body.
--
-- __⚠__ - Violation of the above rule could potentially lead to corrupt memory and segfaults.
--
-- @since 0.1.0
withPtrByteStringST :: BS.ByteString -> (Ptr a -> ST s b) -> ST s b
withPtrByteStringST bs f = withAddrByteStringST bs $ \addr# -> f (Ptr addr#)
{-# INLINE withPtrByteStringST #-}

-- | Same as `withPtrByteStringST`, except the supplied action expects an `Addr#` instead of a
-- `Ptr`.
--
-- __⚠__ - Violation of the rule from `withPtrByteStringST` could potentially lead to corrupt memory
-- and segfaults.
--
-- @since 0.2.0
withAddrByteStringST :: BS.ByteString -> (Addr# -> ST s b) -> ST s b
#if MIN_VERSION_bytestring(0,11,0)
withAddrByteStringST (BS.BS fp _) = withForeignPtrST fp
#else
withAddrByteStringST (BS.PS fp offset _) = withForeignPtrST (fp `plusForeignPtr` offset)
#endif
{-# INLINE withAddrByteStringST #-}

-- | Run an `ST` action on the underlying `Ptr` that points to the beginning of the `ByteString`
-- buffer. It is ok to use ByteString withing ST, as long as underlying pointer is never mutated or
-- returned from the supplied action.
--
-- * __Warning__ - It is important for the memory that backs the underlying `ForeignPtr` to not be
-- mutated outside of the `ST` monad that this action operates in, which is only allowed if it was
-- allocated in this execution of the `ST` monad.
--
-- * __Warning__ - It is important for the memory that backs the underlying `ForeignPtr` to not be
-- mutated at all if `ForeignPtr` was not allocated within the `ST` monad that this action operates
-- in.
--
-- * __Warning__ - It is important for the supplied action to not produce bottom, i.e. runtime
-- exceptions or infinite loops are not allowed within its body.
--
-- __⚠__ - Violation of the above rules could potentially lead to corrupt memory and segfaults.
--
-- @since 0.2.0
withForeignPtrST :: ForeignPtr a -> (Addr# -> ST s b) -> ST s b
withForeignPtrST (ForeignPtr addr# ptrContents) f = do
  !r <- f addr#
  -- It is safe to use `touch#` within ST, so using `unsafeCoerce#` here is totally OK
  ST $ \s# -> (# unsafeCoerce# (touch# ptrContents (unsafeCoerce# s#)), () #)
  pure r
{-# INLINE withForeignPtrST #-}

-- | /O(1)/ - Convert a pinned `ByteArray` to `BS.ByteString`.
--
-- * __Warning__ - There is no check that source `ByteArray` was allocated as pinned, so user of this
-- function must guarantee this invariant.
--
-- __⚠__ - Violation of the above rules could potentially lead to corrupt memory and segfaults.
--
-- @since 0.1.0
pinnedByteArrayToByteString :: ByteArray -> BS.ByteString
pinnedByteArrayToByteString (ByteArray ba#) =
  BS.PS (pinnedByteArrayToForeignPtr ba#) 0 (I# (sizeofByteArray# ba#))
{-# INLINE pinnedByteArrayToByteString #-}

-- | /O(1)/ - Convert a pinned `ByteArray#` to `ForeignPtr`.
--
-- * __Warning__ - There is no check that source `ByteArray#` was allocated as pinned, so user of this
-- function must guarantee this invariant.
--
-- __⚠__ - Violation of the above rules could potentially lead to corrupt memory and segfaults.
--
-- @since 0.1.0
pinnedByteArrayToForeignPtr :: ByteArray# -> ForeignPtr a
pinnedByteArrayToForeignPtr ba# =
  ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#))
{-# INLINE pinnedByteArrayToForeignPtr #-}

-- | /O(1)/ - Convert `ByteArray` to `SBS.ShortByteString`
--
-- @since 0.1.0
byteArrayToShortByteString :: ByteArray -> SBS.ShortByteString
byteArrayToShortByteString (ByteArray ba#) = SBS.SBS ba#
{-# INLINE byteArrayToShortByteString #-}

-- | /O(1)/ - Inverse of `byteArrayToShortByteString`. Convert `SBS.ShortByteString` to  `ByteArray`
--
-- @since 0.1.0
byteArrayFromShortByteString :: SBS.ShortByteString -> ByteArray
byteArrayFromShortByteString (SBS.SBS ba#) = ByteArray ba#
{-# INLINE byteArrayFromShortByteString #-}
