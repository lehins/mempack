{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
module Data.MemPack (
  Pack (..),
  Unpack (..),
  MemPack (..),

  -- * Packing
  pack,
  packByteArray,
  packByteString,
  packShortByteString,
  packMutableByteArray,

  -- ** Helpers
  packIncrement,
  guardAdvanceUnpack,

  -- * Unpacking
  unpack,
  unpackFail,
  unpackMonadFail,
  unpackError,
  unpackLeftOver,

  -- ** Helpers
  failUnpack,
  unpackByteArray,

  -- * Helper packers
  VarLen (..),
  Length (..),
  Tag (..),
  packTagM,
  unpackTagM,
  unknownTagM,
  packedTagByteCount,

  -- * Internal utilities
  replicateTailM,
  lift_#,
  st_,

  -- * Re-exports for GNTD
  StateT (..),
  FailT (..),
) where

#include "MachDeps.h"

import Control.Applicative (Alternative (..))
import Control.Monad (join, unless, when)
import qualified Control.Monad.Fail as F
import Control.Monad.Reader (MonadReader (..), lift)
import Control.Monad.State.Strict (MonadState (..), StateT (..), execStateT)
import Control.Monad.Trans.Fail (Fail, FailT (..), errorFail, failT, runFailAgg)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bifunctor (first)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Semigroup (Sum (..))
import Data.Typeable
import GHC.Exts
import GHC.ST (ST (..), runST)
import GHC.Stack (HasCallStack)
import GHC.Word
import Numeric (showHex)

newtype Pack s a = Pack
  { runPack :: MutableByteArray s -> StateT Int (ST s) a
  }

instance Functor (Pack s) where
  fmap f (Pack p) = Pack $ \buf -> fmap f (p buf)
  {-# INLINE fmap #-}
instance Applicative (Pack s) where
  pure = Pack . const . pure
  {-# INLINE pure #-}
  Pack a1 <*> Pack a2 =
    Pack $ \buf -> a1 buf <*> a2 buf
  {-# INLINE (<*>) #-}
  Pack a1 *> Pack a2 =
    Pack $ \buf -> a1 buf *> a2 buf
  {-# INLINE (*>) #-}
instance Monad (Pack s) where
  Pack m1 >>= p =
    Pack $ \buf -> m1 buf >>= \res -> runPack (p res) buf
  {-# INLINE (>>=) #-}
instance MonadReader (MutableByteArray s) (Pack s) where
  ask = Pack pure
  {-# INLINE ask #-}
  local f (Pack p) = Pack (p . f)
  {-# INLINE local #-}
  reader f = Pack (pure . f)
  {-# INLINE reader #-}
instance MonadState Int (Pack s) where
  get = Pack $ const get
  {-# INLINE get #-}
  put = Pack . const . put
  {-# INLINE put #-}
  state = Pack . const . state
  {-# INLINE state #-}

newtype Unpack b a = Unpack
  { runUnpack :: b -> StateT Int (Fail SomeError) a
  }

instance Functor (Unpack s) where
  fmap f (Unpack p) = Unpack $ \buf -> fmap f (p buf)
  {-# INLINE fmap #-}
instance Applicative (Unpack b) where
  pure = Unpack . const . pure
  {-# INLINE pure #-}
  Unpack a1 <*> Unpack a2 =
    Unpack $ \buf -> a1 buf <*> a2 buf
  {-# INLINE (<*>) #-}
  Unpack a1 *> Unpack a2 =
    Unpack $ \buf -> a1 buf *> a2 buf
  {-# INLINE (*>) #-}
instance Monad (Unpack b) where
  Unpack m1 >>= p =
    Unpack $ \buf -> m1 buf >>= \res -> runUnpack (p res) buf
  {-# INLINE (>>=) #-}
instance F.MonadFail (Unpack b) where
  fail = Unpack . const . F.fail
instance MonadReader b (Unpack b) where
  ask = Unpack pure
  {-# INLINE ask #-}
  local f (Unpack p) = Unpack (p . f)
  {-# INLINE local #-}
  reader f = Unpack (pure . f)
  {-# INLINE reader #-}
instance MonadState Int (Unpack b) where
  get = Unpack $ const get
  {-# INLINE get #-}
  put = Unpack . const . put
  {-# INLINE put #-}
  state = Unpack . const . state
  {-# INLINE state #-}

instance Alternative (Unpack b) where
  empty = Unpack $ \_ -> lift empty
  {-# INLINE empty #-}
  Unpack r1 <|> Unpack r2 =
    Unpack $ \buf ->
      case r1 buf of
        StateT m1 ->
          case r2 buf of
            StateT m2 -> StateT $ \s -> m1 s <|> m2 s
  {-# INLINE (<|>) #-}

failUnpack :: Error e => e -> Unpack b a
failUnpack e = Unpack $ \_ -> lift $ failT (toSomeError e)

class MemPack a where
  typeName :: String
  default typeName :: Typeable a => String
  typeName = show (typeRep (Proxy @a))

  packedByteCount :: a -> Int

  packM :: a -> Pack s ()

  unpackM :: Buffer b => Unpack b a

instance MemPack Int where
  packedByteCount _ = SIZEOF_HSINT
  {-# INLINE packedByteCount #-}
  packM a@(I# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsInt# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_HSINT
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> I# (indexWord8ArrayAsInt# ba# i#))
        (\addr# -> I# (indexIntOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Word where
  packedByteCount _ = SIZEOF_HSWORD
  {-# INLINE packedByteCount #-}
  packM a@(W# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_HSWORD
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> W# (indexWord8ArrayAsWord# ba# i#))
        (\addr# -> W# (indexWordOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Word8 where
  packedByteCount _ = SIZEOF_WORD8
  {-# INLINE packedByteCount #-}
  packM a@(W8# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8Array# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_WORD8
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> W8# (indexWord8Array# ba# i#))
        (\addr# -> W8# (indexWord8OffAddr# addr# i#))
  {-# INLINE unpackM #-}

instance MemPack Word16 where
  packedByteCount _ = SIZEOF_WORD16
  {-# INLINE packedByteCount #-}
  packM a@(W16# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord16# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    buf <- ask
    I# i# <- guardAdvanceUnpack SIZEOF_WORD16
    pure $!
      buffer
        buf
        (\ba# -> W16# (indexWord8ArrayAsWord16# ba# i#))
        (\addr# -> W16# (indexWord16OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Word32 where
  packedByteCount _ = SIZEOF_WORD32
  {-# INLINE packedByteCount #-}
  packM a@(W32# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord32# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_WORD32
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> W32# (indexWord8ArrayAsWord32# ba# i#))
        (\addr# -> W32# (indexWord32OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Word64 where
  packedByteCount _ = SIZEOF_WORD64
  {-# INLINE packedByteCount #-}
  packM a@(W64# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWord64# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_WORD64
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> W64# (indexWord8ArrayAsWord64# ba# i#))
        (\addr# -> W64# (indexWord64OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b) => MemPack (a, b) where
  typeName = "(" ++ typeName @a ++ "," ++ typeName @b ++ ")"
  packedByteCount (a, b) = packedByteCount a + packedByteCount b
  {-# INLINE packedByteCount #-}
  packM (a, b) = do
    packM a
    packM b
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    pure (a, b)
  {-# INLINE unpackM #-}

instance MemPack a => MemPack [a] where
  typeName = "[" ++ typeName @a ++ "]"
  packedByteCount es = packedByteCount (Length (length es)) + getSum (foldMap (Sum . packedByteCount) es)
  {-# INLINE packedByteCount #-}
  packM as = do
    packM (Length (length as))
    mapM_ packM as
  {-# INLINE packM #-}
  unpackM = do
    Length n <- unpackM
    replicateTailM n unpackM
  {-# INLINE unpackM #-}

-- | Tail recursive version of `replicateM`
replicateTailM :: Monad m => Int -> m a -> m [a]
replicateTailM n f = go n []
  where
    go i !acc
      | i <= 0 = pure $ reverse acc
      | otherwise = f >>= \x -> go (i - 1) (x : acc)
{-# INLINE replicateTailM #-}

instance MemPack ByteArray where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  packM ba@(ByteArray ba#) = do
    let !len@(I# len#) = bufferByteCount ba
    packM (Length len)
    I# curPos# <- state $ \i -> (i, i + len)
    MutableByteArray mba# <- ask
    lift_# (copyByteArray# ba# 0# mba# curPos# len#)
  {-# INLINE packM #-}
  unpackM = unpackByteArray False
  {-# INLINE unpackM #-}

instance MemPack ShortByteString where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  packM = packM . byteArrayFromShortByteString
  {-# INLINE packM #-}
  unpackM = byteArrayToShortByteString <$> unpackByteArray False
  {-# INLINE unpackM #-}

instance MemPack ByteString where
  packedByteCount ba =
    let len = bufferByteCount ba
     in packedByteCount (Length len) + len
  {-# INLINE packedByteCount #-}
  packM bs = do
    let !len@(I# len#) = bufferByteCount bs
    packM (Length len)
    I# curPos# <- state $ \i -> (i, i + len)
    Pack $ \(MutableByteArray mba#) -> lift $ withPtrByteStringST bs $ \(Ptr addr#) ->
      st_ (copyAddrToByteArray# addr# mba# curPos# len#)
  {-# INLINE packM #-}
  unpackM = pinnedByteArrayToByteString <$> unpackByteArray True
  {-# INLINE unpackM #-}

unpackByteArray :: Buffer b => Bool -> Unpack b ByteArray
unpackByteArray isPinned = do
  Length len@(I# len#) <- unpackM
  I# curPos# <- guardAdvanceUnpack len
  buf <- ask
  pure $! runST $ do
    mba@(MutableByteArray mba#) <- newMutableByteArray isPinned len
    buffer
      buf
      (\ba# -> st_ (copyByteArray# ba# curPos# mba# 0# len#))
      (\addr# -> st_ (copyAddrToByteArray# (addr# `plusAddr#` curPos#) mba# 0# len#))
    freezeMutableByteArray mba
{-# INLINE unpackByteArray #-}

packIncrement :: MemPack a => a -> Pack s Int
packIncrement a =
  state $ \i ->
    let !n = i + packedByteCount a
     in (i, n)
{-# INLINE packIncrement #-}

guardAdvanceUnpack :: Buffer b => Int -> Unpack b Int
guardAdvanceUnpack n@(I# n#) = do
  buf <- ask
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

-- | Serialize a type into an unpinned `ByteArray`
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> unpack @[Int] $ pack ([1,2,3,4,5] :: [Int])
-- Right [1,2,3,4,5]
pack :: forall a. (MemPack a, HasCallStack) => a -> ByteArray
pack = packByteArray False
{-# INLINE pack #-}

-- | Serialize a type into a pinned `ByteString`
packByteString :: forall a. (MemPack a, HasCallStack) => a -> ByteString
packByteString = pinnedByteArrayToByteString . packByteArray True
{-# INLINE packByteString #-}

-- | Serialize a type into an unpinned `ShortByteString`
packShortByteString :: forall a. (MemPack a, HasCallStack) => a -> ShortByteString
packShortByteString = byteArrayToShortByteString . pack
{-# INLINE packShortByteString #-}

packByteArray :: forall a. (MemPack a, HasCallStack) => Bool -> a -> ByteArray
packByteArray isPinned a =
  runST $ packMutableByteArray isPinned a >>= freezeMutableByteArray
{-# INLINE packByteArray #-}

packMutableByteArray ::
  forall a s. (MemPack a, HasCallStack) => Bool -> a -> ST s (MutableByteArray s)
packMutableByteArray isPinned a = do
  let len = packedByteCount a
  mba <- newMutableByteArray isPinned len
  filledBytes <- execStateT (runPack (packM a) mba) 0
  when (filledBytes /= len) $
    if (filledBytes < len)
      then
        error $
          "Some bug in 'packM' was detected. Buffer of length " <> showBytes len
            ++ " was not fully filled while packing " <> typeName @a
            ++ ". Unfilled " <> showBytes (len - filledBytes) <> "."
      else
        -- This is a critical error, therefore we are not gracefully failing this unpacking
        error $
          "Potential buffer overflow. Some bug in 'packM' was detected while packing " <> typeName @a
            ++ ". Filled " <> showBytes (filledBytes - len) <> " more than allowed into a buffer of length "
            ++ show len
  pure mba
{-# INLINEABLE packMutableByteArray #-}

-- | Unpack a memory `Buffer` into a type using its `MemPack` instance. Besides the
-- unpacked type it also returns an index into a buffer where unpacked has stopped.
unpackLeftOver :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Fail SomeError (a, Int)
unpackLeftOver b = do
  let len = bufferByteCount b
  res@(_, consumedBytes) <- runStateT (runUnpack unpackM b) 0
  when (consumedBytes > len) $
    -- This is a critical error, therefore we are not gracefully failing this unpacking
    error $
      "Potential buffer overflow. Some bug in 'unpackM' was detected while unpacking " <> typeName @a
        ++ ". Consumed " <> showBytes (consumedBytes - len) <> " more than allowed from a buffer of length "
        ++ show len
  pure res
{-# INLINEABLE unpackLeftOver #-}

-- | Unpack a memory `Buffer` into a type using its `MemPack` instance. Besides potential
-- unpacking failures due to a malformed buffer it will also fail the supplied `Buffer`
-- was not fully consumed. Use `unpackLeftOver`, whenever a partially consumed buffer is
-- possible.
unpack :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Either SomeError a
unpack = first fromMultipleErrors . runFailAgg . unpackFail
{-# INLINE unpack #-}

-- | Same as `unpack` except fails in a `Fail` monad, instead of `Either`.
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

-- | Same as `unpackFail` except fails in any `MonadFail`, instead of `Fail`.
unpackMonadFail :: forall a b m. (MemPack a, Buffer b, F.MonadFail m) => b -> m a
unpackMonadFail = either (F.fail . show) pure . unpack
{-# INLINE unpackMonadFail #-}

-- | Same as `unpack` except throws a runtime exception upon a failure
unpackError :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> a
unpackError = errorFail . unpackFail
{-# INLINE unpackError #-}

newtype VarLen a = VarLen {unVarLen :: a}
  deriving (Eq, Ord, Show, Bounded, Enum, Num, Real, Integral, Bits, FiniteBits)

instance MemPack (VarLen Word16) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  packM v@(VarLen x) = p7 (p7 (p7 (errorTooManyBits "Word16"))) (numBits - 7)
    where
      p7 = packIntoCont7 x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE packM #-}
  unpackM = do
    let d7 = unpack7BitVarLen
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (unpack7BitVarLenLast 0b_1111_1100)) 0 0
  {-# INLINE unpackM #-}

instance MemPack (VarLen Word32) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  packM v@(VarLen x) = p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word32"))))) (numBits - 7)
    where
      p7 = packIntoCont7 x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE packM #-}
  unpackM = do
    let d7 = unpack7BitVarLen
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (unpack7BitVarLenLast 0b_1111_0000)))) 0 0
  {-# INLINE unpackM #-}

instance MemPack (VarLen Word64) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
  packM v@(VarLen x) =
    p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word64")))))))))) (numBits - 7)
    where
      p7 = packIntoCont7 x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE packM #-}
  unpackM = do
    let d7 = unpack7BitVarLen
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (unpack7BitVarLenLast 0b_1111_1110))))))))) 0 0
  {-# INLINE unpackM #-}

instance MemPack (VarLen Word) where
  packedByteCount = packedVarLenByteCount
  {-# INLINE packedByteCount #-}
#if WORD_SIZE_IN_BITS == 32
  packM mba v@(VarLen x) = p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word"))))) (numBits - 7)
    where
      p7 = packIntoCont7 mba x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE packM #-}
  unpackM buf = do
    let d7 = unpack7BitVarLen buf
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (unpack7BitVarLenLast buf 0b_1111_0000)))) 0 0
  {-# INLINE unpackM #-}
#elif WORD_SIZE_IN_BITS == 64
  packM v@(VarLen x) =
    p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (p7 (errorTooManyBits "Word")))))))))) (numBits - 7)
    where
      p7 = packIntoCont7 x
      {-# INLINE p7 #-}
      numBits = packedVarLenByteCount v * 7
  {-# INLINE packM #-}
  unpackM = do
    let d7 = unpack7BitVarLen
        {-# INLINE d7 #-}
    VarLen <$> d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (d7 (unpack7BitVarLenLast 0b_1111_1110))))))))) 0 0
  {-# INLINE unpackM #-}
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

errorTooManyBits :: [Char] -> a
errorTooManyBits name =
  error $ "Bug detected. Trying to pack more bits for " ++ name ++ " than it should be posssible"

packIntoCont7 ::
  (Bits t, Integral t) => t -> (Int -> Pack s ()) -> Int -> Pack s ()
packIntoCont7 x cont n
  | n <= 0 = packM (fromIntegral @_ @Word8 x .&. complement topBit8)
  | otherwise = do
      packM (fromIntegral @_ @Word8 (x `shiftR` n) .|. topBit8)
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
  -- | Continuation that will be invoked if MSB is set
  (Word8 -> a -> Unpack b a) ->
  -- | Will be set either to 0 initially or to the very first unmodified byte, which is
  -- guaranteed to have the first bit set.
  Word8 ->
  -- | Accumulator
  a ->
  Unpack b a
unpack7BitVarLen cont firstByte !acc = do
  b8 :: Word8 <- unpackM
  if b8 `testBit` 7
    then
      cont (if firstByte == 0 then b8 else firstByte) (acc `shiftL` 7 .|. fromIntegral (b8 `clearBit` 7))
    else pure (acc `shiftL` 7 .|. fromIntegral b8)
{-# INLINE unpack7BitVarLen #-}

unpack7BitVarLenLast ::
  forall t b.
  (Num t, Bits t, MemPack t, Buffer b) =>
  Word8 ->
  Word8 ->
  t ->
  Unpack b t
unpack7BitVarLenLast mask firstByte acc = do
  res <- unpack7BitVarLen (\_ _ -> F.fail "Too many bytes.") firstByte acc
  -- Only while decoding the last 7bits we check if there was too many
  -- bits supplied at the beginning.
  unless (firstByte .&. mask == 0b_1000_0000) $
    F.fail $
      "Unexpected bits for "
        ++ typeName @t
        ++ " were set in the first byte of 'VarLen': 0x" <> showHex firstByte ""
  pure res
{-# INLINE unpack7BitVarLenLast #-}

newtype Length = Length {unLength :: Int}
  deriving (Eq, Show, Num)

instance MemPack Length where
  packedByteCount = packedByteCount . VarLen . fromIntegral @Int @Word . unLength
  packM (Length n)
    | n < 0 = error "Length cannot be negative"
    | otherwise = packM (VarLen (fromIntegral @Int @Word n))
  {-# INLINE packM #-}
  unpackM = do
    VarLen (w :: Word) <- unpackM
    when (testBit w (finiteBitSize w)) $ F.fail "Attempt to unpack negative length was detected"
    pure $ Length $ fromIntegral @Word @Int w
  {-# INLINE unpackM #-}

newtype Tag = Tag {unTag :: Word8}
  deriving (Eq, Ord, Show, Num)

-- Manually defined instance, since ghc-8.6 has issues with deriving MemPack
instance MemPack Tag where
  packedByteCount _ = packedTagByteCount
  {-# INLINE packedByteCount #-}
  unpackM = unpackTagM
  {-# INLINE unpackM #-}
  packM = packTagM
  {-# INLINE packM #-}

packedTagByteCount :: Int
packedTagByteCount = 1

unpackTagM :: Buffer b => Unpack b Tag
unpackTagM = Tag <$> unpackM
{-# INLINE unpackTagM #-}

packTagM :: Tag -> Pack s ()
packTagM = packM . unTag
{-# INLINE packTagM #-}

unknownTagM :: F.MonadFail m => Tag -> m a
unknownTagM (Tag t) = F.fail $ "Unrecognized Tag: " ++ show t

lift_# :: (State# s -> State# s) -> Pack s ()
lift_# f = Pack $ \_ -> lift $ st_ f
{-# INLINE lift_# #-}

st_ :: (State# s -> State# s) -> ST s ()
st_ f = ST $ \s# -> (# f s#, () #)
{-# INLINE st_ #-}
