{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.MemPack
-- Copyright   : (c) Alexey Kuleshevich 2024
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack where

#include "MachDeps.h"

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Fail
import Data.Array.Byte
import Data.Bifunctor (first)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Proxy
import Data.Semigroup (Sum (..))
import Data.Typeable
import GHC.Exts
import GHC.ST
import GHC.Stack
import GHC.Word
import Numeric (showHex)

newtype Pack s a = Pack
  { runPack :: StateT Int (ST s) a
  }
  deriving (Functor, Applicative, Monad, MonadState Int)

newtype Unpack a = Unpack
  { runUnpack :: StateT Int (Fail SomeError) a
  }
  deriving (Functor, Applicative, Monad, MonadFail, MonadState Int)

instance Alternative Unpack where
  empty = Unpack $ lift empty
  Unpack (StateT m1) <|> Unpack (StateT m2) =
    Unpack $ StateT $ \s -> m1 s <|> m2 s

failUnpack :: Error e => e -> Unpack a
failUnpack = Unpack . lift . failT . toSomeError

class MemPack a where
  typeName :: String
  default typeName :: Typeable a => String
  typeName = show (typeRep (Proxy @a))

  packedByteCount :: a -> Int

  unsafePackInto :: MutableByteArray s -> a -> Pack s ()

  unpackBuffer :: Buffer b => b -> Unpack a

instance MemPack Int where
  packedByteCount _ = SIZEOF_HSINT
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(I# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsInt# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_HSINT
    pure $!
      buffer
        b
        (\ba# -> I# (indexWord8ArrayAsInt# ba# i#))
        (\addr# -> I# (indexIntOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackBuffer #-}

instance MemPack Word where
  packedByteCount _ = SIZEOF_HSWORD
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(W# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_HSWORD
    pure $!
      buffer
        b
        (\ba# -> W# (indexWord8ArrayAsWord# ba# i#))
        (\addr# -> W# (indexWordOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackBuffer #-}

instance MemPack Word8 where
  packedByteCount _ = SIZEOF_WORD8
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(W8# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8Array# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_WORD8
    pure $!
      buffer
        b
        (\ba# -> W8# (indexWord8Array# ba# i#))
        (\addr# -> W8# (indexWord8OffAddr# addr# i#))
  {-# INLINE unpackBuffer #-}

instance MemPack Word16 where
  packedByteCount _ = SIZEOF_WORD16
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(W16# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord16# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_WORD16
    pure $!
      buffer
        b
        (\ba# -> W16# (indexWord8ArrayAsWord16# ba# i#))
        (\addr# -> W16# (indexWord16OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackBuffer #-}

instance MemPack Word32 where
  packedByteCount _ = SIZEOF_WORD32
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(W32# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord32# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_WORD32
    pure $!
      buffer
        b
        (\ba# -> W32# (indexWord8ArrayAsWord32# ba# i#))
        (\addr# -> W32# (indexWord32OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackBuffer #-}

instance MemPack Word64 where
  packedByteCount _ = SIZEOF_WORD64
  {-# INLINE packedByteCount #-}
  unsafePackInto (MutableByteArray mba#) a@(W64# a#) = do
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord64# mba# i# a#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer b = do
    I# i# <- guardAdvanceUnpack b SIZEOF_WORD64
    pure $!
      buffer
        b
        (\ba# -> W64# (indexWord8ArrayAsWord64# ba# i#))
        (\addr# -> W64# (indexWord64OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackBuffer #-}

instance (MemPack a, MemPack b) => MemPack (a, b) where
  typeName = "(" ++ typeName @a ++ "," ++ typeName @b ++ ")"
  packedByteCount (a, b) = packedByteCount a + packedByteCount b
  {-# INLINE packedByteCount #-}
  unsafePackInto mba (a, b) = do
    unsafePackInto mba a
    unsafePackInto mba b
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    !a <- unpackBuffer buf
    !b <- unpackBuffer buf
    pure (a, b)
  {-# INLINE unpackBuffer #-}

instance MemPack a => MemPack [a] where
  typeName = "[" ++ typeName @a ++ "]"
  packedByteCount es = packedByteCount (Length (length es)) + getSum (foldMap (Sum . packedByteCount) es)
  {-# INLINE packedByteCount #-}
  unsafePackInto mba as = do
    unsafePackInto mba (Length (length as))
    mapM_ (unsafePackInto mba) as
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    Length n <- unpackBuffer buf
    replicateM n (unpackBuffer buf)
  {-# INLINE unpackBuffer #-}

instance MemPack ByteArray where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  unsafePackInto mba@(MutableByteArray mba#) ba@(ByteArray ba#) = do
    let !len@(I# len#) = bufferByteCount ba
    unsafePackInto mba (Length len)
    I# curPos# <- state $ \i -> (i, i + len)
    lift_# (copyByteArray# ba# 0# mba# curPos# len#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer = unpackByteArray False
  {-# INLINE unpackBuffer #-}

instance MemPack ShortByteString where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  unsafePackInto mba = unsafePackInto mba . byteArrayFromShortByteString
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = byteArrayToShortByteString <$> unpackByteArray False buf
  {-# INLINE unpackBuffer #-}

instance MemPack ByteString where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  unsafePackInto mba@(MutableByteArray mba#) bs = do
    let !len@(I# len#) = bufferByteCount bs
    unsafePackInto mba (Length len)
    I# curPos# <- state $ \i -> (i, i + len)
    Pack $ lift $ withPtrByteStringST bs $ \(Ptr addr#) ->
      st_ (copyAddrToByteArray# addr# mba# curPos# len#)
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = pinnedByteArrayToByteString <$> unpackByteArray True buf
  {-# INLINE unpackBuffer #-}

unpackByteArray :: Buffer b => Bool -> b -> Unpack ByteArray
unpackByteArray isPinned buf = do
  Length len@(I# len#) <- unpackBuffer buf
  I# curPos# <- guardAdvanceUnpack buf len
  pure $! runST $ do
    mba@(MutableByteArray mba#) <- newMutableByteArray isPinned len
    buffer
      buf
      (\ba# -> st_ (copyByteArray# ba# curPos# mba# 0# len#))
      (\addr# -> st_ (copyAddrToByteArray# (addr# `plusAddr#` curPos#) mba# 0# len#))
    freezeMutableByteArray mba
{-# INLINE unpackByteArray #-}

lift_# :: (State# s -> State# s) -> Pack s ()
lift_# f = Pack $ lift $ ST $ \s# -> (# f s#, () #)
{-# INLINE lift_# #-}

st_ :: (State# s -> State# s) -> ST s ()
st_ f = ST $ \s# -> (# f s#, () #)
{-# INLINE st_ #-}

packIncrement :: MemPack a => a -> Pack s Int
packIncrement a =
  state $ \i ->
    let !n = i + packedByteCount a
     in (i, n)
{-# INLINE packIncrement #-}

guardAdvanceUnpack :: Buffer b => b -> Int -> Unpack Int
guardAdvanceUnpack buf n@(I# n#) = do
  let len = bufferByteCount buf
      failOutOfBytes i =
        failUnpack $
          toSomeError $
            RanOutOfBytesError
              { ranOutOfBytesRead = i
              , ranOutOfBytesAvailable = len
              , ranOutOfBytesRequested = n
              }
  -- Check that we still have enough bytes, while guarding against integer overflow.
  join $ state $ \i@(I# i#) ->
    case addIntC# i# n# of
      (# adv#, 0# #) ->
        if len < I# adv#
          then (failOutOfBytes i, i)
          else (pure i, I# adv#)
      _ -> (failOutOfBytes i, i)
{-# INLINE guardAdvanceUnpack #-}

pack :: forall a. (MemPack a, HasCallStack) => a -> ByteArray
pack = packByteArray False
{-# INLINE pack #-}

packByteString :: forall a. (MemPack a, HasCallStack) => a -> ByteString
packByteString = pinnedByteArrayToByteString . packByteArray True
{-# INLINE packByteString #-}

packShortByteString :: forall a. (MemPack a, HasCallStack) => a -> ShortByteString
packShortByteString = byteArrayToShortByteString . pack
{-# INLINE packShortByteString #-}

packByteArray :: forall a. (MemPack a, HasCallStack) => Bool -> a -> ByteArray
packByteArray isPinned a =
  runST $
    packMutableByteArray isPinned a >>= freezeMutableByteArray
{-# INLINE packByteArray #-}

freezeMutableByteArray :: MutableByteArray d -> ST d ByteArray
freezeMutableByteArray (MutableByteArray mba#) =
  ST $ \s# -> case unsafeFreezeByteArray# mba# s# of
    (# s'#, ba# #) -> (# s'#, ByteArray ba# #)

newMutableByteArray :: Bool -> Int -> ST s (MutableByteArray s)
newMutableByteArray isPinned (I# len#) =
  ST $ \s# -> case (if isPinned then newPinnedByteArray# else newByteArray#) len# s# of
    (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
{-# INLINE newMutableByteArray #-}

packMutableByteArray ::
  forall a s. (MemPack a, HasCallStack) => Bool -> a -> ST s (MutableByteArray s)
packMutableByteArray isPinned a = do
  let len = packedByteCount a
  mba <- newMutableByteArray isPinned len
  filledBytes <- execStateT (runPack (unsafePackInto mba a)) 0
  when (filledBytes /= len) $
    if (filledBytes < len)
      then
        error $
          "Some bug in 'unsafePackInto' was detected. Buffer of length " <> showBytes len
            ++ " was not fully filled while packing " <> typeName @a
            ++ ". Unfilled " <> showBytes (len - filledBytes) <> "."
      else
        -- This is a critical error, therefore we are not gracefully failing this unpacking
        error $
          "Potential buffer overflow. Some bug in 'unsafePackInto' was detected while packing " <> typeName @a
            ++ ". Filled " <> showBytes (filledBytes - len) <> " more than allowed into a buffer of length "
            ++ show len
  pure mba
{-# INLINEABLE packMutableByteArray #-}

unpackLeftOver :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Fail SomeError (a, Int)
unpackLeftOver b = do
  let len = bufferByteCount b
  res@(_, consumedBytes) <- runStateT (runUnpack (unpackBuffer b)) 0
  when (consumedBytes > len) $
    -- This is a critical error, therefore we are not gracefully failing this unpacking
    error $
      "Potential buffer overflow. Some bug in 'unpackBuffer' was detected while unpacking " <> typeName @a
        ++ ". Consumed " <> showBytes (consumedBytes - len) <> " more than allowed from a buffer of length "
        ++ show len
  pure res
{-# INLINEABLE unpackLeftOver #-}

unpack :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Either SomeError a
unpack = first fromMultipleErrors . runFailAgg . unpackFail
{-# INLINE unpack #-}

unpackFail :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Fail SomeError a
unpackFail b = do
  let len = bufferByteCount b
  (a, consumedBytes) <- unpackLeftOver b
  when (consumedBytes /= len) $
    failT $
      toSomeError $
        NotFullyConsumedError
          { notFullyConsumedRead = consumedBytes
          , notFullyConsumedAvailable = len
          , notFullyConsumedTypeName = typeName @a
          }
  pure a
{-# INLINEABLE unpackFail #-}

unpackError :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> a
unpackError = errorFail . unpackFail
{-# INLINE unpackError #-}

newtype VarLen a = VarLen {unVarLen :: a}
  deriving (Eq, Ord, Show, Bounded, Enum, Num, Real, Integral, Bits, FiniteBits)

instance MemPack (VarLen Word16) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  unsafePackInto mba v@(VarLen x) = p7 (p7 (p7 (errorTooManyBits "Word16"))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (unpack7BitVarLenLast buf 0b_1111_1100)) 0 0
  {-# INLINE unpackBuffer #-}

instance MemPack (VarLen Word32) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  unsafePackInto mba v@(VarLen x) = p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word32"))))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (unpack7BitVarLenLast buf 0b_1111_0000)))) 0 0
  {-# INLINE unpackBuffer #-}

instance MemPack (VarLen Word64) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  unsafePackInto mba v@(VarLen x) =
    p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word64")))))))))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (unpack7BitVarLenLast buf 0b_1111_1110))))))))) 0 0
  {-# INLINE unpackBuffer #-}

instance MemPack (VarLen Word) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
#if WORD_SIZE_IN_BITS == 32
  unsafePackInto mba v@(VarLen x) = p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word"))))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (unpack7BitVarLenLast buf 0b_1111_0000)))) 0 0
  {-# INLINE unpackBuffer #-}
#elif WORD_SIZE_IN_BITS == 64
  unsafePackInto mba v@(VarLen x) =
    p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word")))))))))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (unpack7BitVarLenLast buf 0b_1111_1110))))))))) 0 0
  {-# INLINE unpackBuffer #-}
#else
#error "Only 32bit and 64bit systems are supported"
#endif

packedVarLenByteCount :: FiniteBits b => VarLen b -> Int
packedVarLenByteCount (VarLen x) =
  case (finiteBitSize x - countLeadingZeros x) `quotRem` 7 of
    (0, 0) -> 1
    (q, 0) -> q
    (q, _) -> q + 1
{-# INLINE packedVarLenByteCount #-}

packIntoVarLenBits ::
  (Integral t, FiniteBits t) => MutableByteArray s -> VarLen t -> Pack s ()
packIntoVarLenBits mba v@(VarLen x) = go numBits
  where
    topBit8 :: Word8
    topBit8 = 0b_1000_0000
    numBits = packedVarLenByteCount v * 7 - 7
    go n
      | n <= 0 = unsafePackInto mba (fromIntegral @_ @Word8 x .&. complement topBit8)
      | otherwise = do
          unsafePackInto mba (fromIntegral @_ @Word8 (x `shiftR` n) .|. topBit8)
          go (n - 7)
{-# INLINE packIntoVarLenBits #-}

errorTooManyBits :: [Char] -> a
errorTooManyBits name =
  error $ "Bug detected. Trying to pack more bits for " ++ name ++ " than it should be posssible"

packIntoCont7 ::
  (Bits t, Integral t) => MutableByteArray s -> t -> (Int -> Pack s ()) -> Int -> Pack s ()
packIntoCont7 mba x cont n
  | n <= 0 = unsafePackInto mba (fromIntegral @_ @Word8 x .&. complement topBit8)
  | otherwise = do
      unsafePackInto mba (fromIntegral @_ @Word8 (x `shiftR` n) .|. topBit8)
      cont (n - 7)
  where
    topBit8 :: Word8
    topBit8 = 0b_1000_0000
{-# INLINE packIntoCont7 #-}

-- | Decode a variable length integral value that is encoded with 7 bits of data
-- and the most significant bit (MSB), the 8th bit is set whenever there are
-- more bits following. Continuation style allows us to avoid
-- recursion. Removing loops is good for performance.
unpack7BitVarLen ::
  (Num a, Bits a, Buffer b) =>
  -- | Buffer that contains encoded number
  b ->
  -- | Continuation that will be invoked if MSB is set
  (Word8 -> a -> Unpack a) ->
  -- | Will be set either to 0 initially or to the very first unmodified byte, which is
  -- guaranteed to have the first bit set.
  Word8 ->
  -- | Accumulator
  a ->
  Unpack a
unpack7BitVarLen buf cont firstByte !acc = do
  b8 :: Word8 <- unpackBuffer buf
  if b8 `testBit` 7
    then
      cont (if firstByte == 0 then b8 else firstByte) (acc `shiftL` 7 .|. fromIntegral (b8 `clearBit` 7))
    else pure (acc `shiftL` 7 .|. fromIntegral b8)
{-# INLINE unpack7BitVarLen #-}

unpack7BitVarLenLast ::
  forall t b.
  (Num t, Bits t, MemPack t, Buffer b) =>
  b ->
  Word8 ->
  Word8 ->
  t ->
  Unpack t
unpack7BitVarLenLast buf mask firstByte acc = do
  res <- unpack7BitVarLen buf (\_ _ -> fail "Too many bytes.") firstByte acc
  -- Only while decoding the last 7bits we check if there was too many
  -- bits supplied at the beginning.
  unless (firstByte .&. mask == 0b_1000_0000) $
    fail $
      "Unexpected bits for "
        ++ typeName @t
        ++ " were set in the first byte of 'VarLen': 0x" <> showHex firstByte ""
  pure res
{-# INLINE unpack7BitVarLenLast #-}

newtype Length = Length {unLength :: Int}
  deriving (Eq, Show, Num)

instance MemPack Length where
  packedByteCount = packedByteCount . VarLen . fromIntegral @Int @Word . unLength
  unsafePackInto buf (Length n)
    | n < 0 = error "Length cannot be negative"
    | otherwise = unsafePackInto buf (VarLen (fromIntegral @Int @Word n))
  {-# INLINE unsafePackInto #-}

  unpackBuffer buf = do
    VarLen (w :: Word) <- unpackBuffer buf
    when (testBit w (finiteBitSize w)) $ fail "Attempt to unpack negative length was detected"
    pure $ Length $ fromIntegral @Word @Int w
  {-# INLINE unpackBuffer #-}
