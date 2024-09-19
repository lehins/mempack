{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * Re-exports for @GeneralizedNewtypeDeriving@
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
import Data.Char (ord)
import Data.Complex (Complex (..))
import Data.List (intercalate)
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Ratio
import Data.Semigroup (Sum (..))
import Data.Typeable
import GHC.Exts
import GHC.Int
import GHC.ST (ST (..), runST)
import GHC.Stable (StablePtr (..))
import GHC.Stack (HasCallStack)
import GHC.Word
import Numeric (showHex)
import Prelude hiding (fail)
#if __GLASGOW_HASKELL__ >= 900
import GHC.Num.Integer (Integer (..), integerCheck)
import GHC.Num.Natural (Natural (..), naturalCheck)
#elif defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals (Integer (..), BigNat(BN#), isValidInteger#)
import GHC.Natural (Natural (..), isValidNatural)
#else
#error "Only integer-gmp is supported for now for older compilers"
#endif
#if !(MIN_VERSION_base(4,13,0))
import Prelude (fail)
#endif

-- | Monad that is used for serializing data into a `MutableByteArray`. It is based on
-- `StateT` that tracks the current index into the `MutableByteArray` where next write is
-- expected to happen.
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

-- | Monad that is used for deserializing data from a memory `Buffer`. It is based on
-- `StateT` that tracks the current index into the @`Buffer` a@, from where the next read
-- suppose to happen. Unpacking can `F.fail` with `F.MonadFail` instance or with
-- `failUnpack` that provides a more type safe way of failing using `Error` interface.
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
#if !(MIN_VERSION_base(4,13,0))
  fail = Unpack . const . F.fail
#endif
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

-- | Failing unpacking with an `Error`.
failUnpack :: Error e => e -> Unpack b a
failUnpack e = Unpack $ \_ -> lift $ failT (toSomeError e)

-- | Efficient serialization interface that operates directly on memory buffers.
class MemPack a where
  -- | Name of the type that is being deserialized for error reporting. Default
  -- implementation relies on `Typeable`.
  typeName :: String
  default typeName :: Typeable a => String
  typeName = show (typeRep (Proxy @a))

  -- | Report the exact size in number of bytes that packed version of this type will
  -- occupy. It is very important to get this right, otherwise `packM` will result in a
  -- runtime exception. Another words this is the expected property that it should hold:
  --
  -- prop> packedByteCount a == bufferByteCount (pack a)
  packedByteCount :: a -> Int

  -- | Write binary representation of a type into the `MutableByteArray` which can be
  -- accessed with `ask`, whenever direct operations on it are necessary.
  packM :: a -> Pack s ()

  -- | Read binary representation of the type directly from the buffer, which can be
  -- accessed with `ask` when necessary. Direct reads from the buffer should be preceded
  -- with advancing the buffer offset with `MonadState` by the number of bytes that will
  -- be consumed from the buffer and making sure that no reads outside of the buffer can
  -- happen. Violation of these rules will lead to segfaults.
  unpackM :: Buffer b => Unpack b a

instance MemPack Char where
  packedByteCount _ = SIZEOF_HSCHAR
  {-# INLINE packedByteCount #-}
  packM a@(C# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsWideChar# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_HSCHAR
    buf <- ask
    let c =
          buffer
            buf
            (\ba# -> C# (indexWord8ArrayAsWideChar# ba# i#))
            (\addr# -> C# (indexWideCharOffAddr# (addr# `plusAddr#` i#) 0#))
    when (ord c > 0x10FFFF) $
      F.fail $
        "Out of bounds Char was detected: '\\x" ++ showHex (fromEnum c) "'"
    pure c
  {-# INLINE unpackM #-}

instance MemPack Float where
  packedByteCount _ = SIZEOF_FLOAT
  {-# INLINE packedByteCount #-}
  packM a@(F# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsFloat# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_FLOAT
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> F# (indexWord8ArrayAsFloat# ba# i#))
        (\addr# -> F# (indexFloatOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Double where
  packedByteCount _ = SIZEOF_DOUBLE
  {-# INLINE packedByteCount #-}
  packM a@(D# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsDouble# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_DOUBLE
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> D# (indexWord8ArrayAsDouble# ba# i#))
        (\addr# -> D# (indexDoubleOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack (Ptr a) where
  typeName = "Ptr"
  packedByteCount _ = SIZEOF_HSPTR
  {-# INLINE packedByteCount #-}
  packM a@(Ptr a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsAddr# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_HSPTR
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> Ptr (indexWord8ArrayAsAddr# ba# i#))
        (\addr# -> Ptr (indexAddrOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack (StablePtr a) where
  typeName = "StablePtr"
  packedByteCount _ = SIZEOF_HSSTABLEPTR
  {-# INLINE packedByteCount #-}
  packM a@(StablePtr a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsStablePtr# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_HSSTABLEPTR
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> StablePtr (indexWord8ArrayAsStablePtr# ba# i#))
        (\addr# -> StablePtr (indexStablePtrOffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

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

instance MemPack Int8 where
  packedByteCount _ = SIZEOF_INT8
  {-# INLINE packedByteCount #-}
  packM a@(I8# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeInt8Array# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    I# i# <- guardAdvanceUnpack SIZEOF_INT8
    buf <- ask
    pure $!
      buffer
        buf
        (\ba# -> I8# (indexInt8Array# ba# i#))
        (\addr# -> I8# (indexInt8OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Int16 where
  packedByteCount _ = SIZEOF_INT16
  {-# INLINE packedByteCount #-}
  packM a@(I16# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsInt16# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    buf <- ask
    I# i# <- guardAdvanceUnpack SIZEOF_INT16
    pure $!
      buffer
        buf
        (\ba# -> I16# (indexWord8ArrayAsInt16# ba# i#))
        (\addr# -> I16# (indexInt16OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Int32 where
  packedByteCount _ = SIZEOF_INT32
  {-# INLINE packedByteCount #-}
  packM a@(I32# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsInt32# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    buf <- ask
    I# i# <- guardAdvanceUnpack SIZEOF_INT32
    pure $!
      buffer
        buf
        (\ba# -> I32# (indexWord8ArrayAsInt32# ba# i#))
        (\addr# -> I32# (indexInt32OffAddr# (addr# `plusAddr#` i#) 0#))
  {-# INLINE unpackM #-}

instance MemPack Int64 where
  packedByteCount _ = SIZEOF_INT64
  {-# INLINE packedByteCount #-}
  packM a@(I64# a#) = do
    MutableByteArray mba# <- ask
    I# i# <- packIncrement a
    lift_# (writeWord8ArrayAsInt64# mba# i# a#)
  {-# INLINE packM #-}
  unpackM = do
    buf <- ask
    I# i# <- guardAdvanceUnpack SIZEOF_INT64
    pure $!
      buffer
        buf
        (\ba# -> I64# (indexWord8ArrayAsInt64# ba# i#))
        (\addr# -> I64# (indexInt64OffAddr# (addr# `plusAddr#` i#) 0#))
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

#if __GLASGOW_HASKELL__ >= 900
instance MemPack Integer where
  packedByteCount =
    (+ packedTagByteCount) . \case
      IS i# -> packedByteCount (I# i#)
      IP ba# -> packedByteCount (ByteArray ba#)
      IN ba# -> packedByteCount (ByteArray ba#)
  {-# INLINE packedByteCount #-}
  packM = \case
    IS i# -> packTagM 0 >> packM (I# i#)
    IP ba# -> packTagM 1 >> packM (ByteArray ba#)
    IN ba# -> packTagM 2 >> packM (ByteArray ba#)
  {-# INLINE packM #-}
  unpackM = do
    i <-
      unpackTagM >>= \case
        0 -> do
          I# i# <- unpackM
          pure $ IS i#
        1 -> do
          ByteArray ba# <- unpackM
          pure $ IP ba#
        2 -> do
          ByteArray ba# <- unpackM
          pure $ IN ba#
        t -> unknownTagM t
    unless (integerCheck i) $ F.fail $ "Invalid Integer decoded " ++ showInteger i
    pure i
    where
      showInteger = \case
        IS i# -> "IS " ++ show (I# i#)
        IP ba# -> "IP " ++ show (ByteArray ba#)
        IN ba# -> "IN " ++ show (ByteArray ba#)
  {-# INLINE unpackM #-}

instance MemPack Natural where
  packedByteCount =
    (+ packedTagByteCount) . \case
      NS w# -> packedByteCount (W# w#)
      NB ba# -> packedByteCount (ByteArray ba#)
  {-# INLINE packedByteCount #-}
  packM = \case
    NS w# -> packTagM 0 >> packM (W# w#)
    NB ba# -> packTagM 1 >> packM (ByteArray ba#)
  {-# INLINE packM #-}
  unpackM = do
    n <-
      unpackTagM >>= \case
        0 -> do
          W# w# <- unpackM
          pure $ NS w#
        1 -> do
          ByteArray ba# <- unpackM
          pure $ NB ba#
        t -> unknownTagM t
    unless (naturalCheck n) $ F.fail $ "Invalid Natural decoded " ++ showNatural n
    pure n
    where
      showNatural = \case
        NS w# -> "NS " ++ show (W# w#)
        NB ba# -> "NB " ++ show (ByteArray ba#)
  {-# INLINE unpackM #-}

#elif defined(MIN_VERSION_integer_gmp)

instance MemPack Integer where
  packedByteCount =
    (+ packedTagByteCount) . \case
      S# i# -> packedByteCount (I# i#)
      Jp# (BN# ba#) -> packedByteCount (ByteArray ba#)
      Jn# (BN# ba#) -> packedByteCount (ByteArray ba#)
  {-# INLINE packedByteCount #-}
  packM = \case
    S# i# -> packTagM 0 >> packM (I# i#)
    Jp# (BN# ba#) -> packTagM 1 >> packM (ByteArray ba#)
    Jn# (BN# ba#) -> packTagM 2 >> packM (ByteArray ba#)
  {-# INLINE packM #-}
  unpackM = do
    i <-
      unpackTagM >>= \case
        0 -> do
          I# i# <- unpackM
          pure $ S# i#
        1 -> do
          ByteArray ba# <- unpackM
          pure $ Jp# (BN# ba#)
        2 -> do
          ByteArray ba# <- unpackM
          pure $ Jn# (BN# ba#)
        t -> unknownTagM t
    unless (isTrue# (isValidInteger# i)) $ F.fail $ "Invalid Integer decoded " ++ showInteger i
    pure i
    where
      showInteger = \case
        S# i# -> "S# " ++ show (I# i#)
        Jp# (BN# ba#) -> "Jp# " ++ show (ByteArray ba#)
        Jn# (BN# ba#) -> "Jn# " ++ show (ByteArray ba#)
  {-# INLINE unpackM #-}

instance MemPack Natural where
  packedByteCount =
    (+ packedTagByteCount) . \case
      NatS# w# -> packedByteCount (W# w#)
      NatJ# (BN# ba#) -> packedByteCount (ByteArray ba#)
  {-# INLINE packedByteCount #-}
  packM = \case
    NatS# w# -> packTagM 0 >> packM (W# w#)
    NatJ# (BN# ba#) -> packTagM 1 >> packM (ByteArray ba#)
  {-# INLINE packM #-}
  unpackM = do
    n <-
      unpackTagM >>= \case
        0 -> do
          W# w# <- unpackM
          pure $ NatS# w#
        1 -> do
          ByteArray ba# <- unpackM
          pure $ NatJ# (BN# ba#)
        t -> unknownTagM t
    unless (isValidNatural n) $ F.fail $ "Invalid Natural decoded " ++ showNatural n
    pure n
    where
      showNatural = \case
        NatS# w# -> "NatS# " ++ show (W# w#)
        NatJ# (BN#  ba#) -> "NatJ# " ++ show (ByteArray ba#)
  {-# INLINE unpackM #-}

#endif

instance MemPack a => MemPack (Complex a) where
  typeName = "Complex " ++ typeName @a
  packedByteCount (a :+ b) = packedByteCount a + packedByteCount b
  {-# INLINE packedByteCount #-}
  packM (a :+ b) = packM a >> packM b
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    pure (a :+ b)
  {-# INLINE unpackM #-}

instance (MemPack a, Integral a) => MemPack (Ratio a) where
  typeName = "Ratio " ++ typeName @a
  packedByteCount r = packedByteCount (numerator r) + packedByteCount (denominator r)
  {-# INLINE packedByteCount #-}
  packM r = packM (numerator r) >> packM (denominator r)
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    when (b == 0) $ F.fail $ "Zero denominator was detected when unpacking " ++ typeName @(Ratio a)
    pure (a % b)
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b) => MemPack (a, b) where
  typeName = "(" ++ typeName @a ++ "," ++ typeName @b ++ ")"
  packedByteCount (a, b) = packedByteCount a + packedByteCount b
  {-# INLINE packedByteCount #-}
  packM (a, b) = packM a >> packM b
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    pure (a, b)
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b, MemPack c) => MemPack (a, b, c) where
  typeName = "(" ++ typeName @a ++ "," ++ typeName @b ++ "," ++ typeName @c ++ ")"
  packedByteCount (a, b, c) = packedByteCount a + packedByteCount b + packedByteCount c
  {-# INLINE packedByteCount #-}
  packM (a, b, c) = packM a >> packM b >> packM c
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    !c <- unpackM
    pure (a, b, c)
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b, MemPack c, MemPack d) => MemPack (a, b, c, d) where
  typeName = "(" ++ typeName @a ++ "," ++ typeName @b ++ "," ++ typeName @c ++ "," ++ typeName @d ++ ")"
  packedByteCount (a, b, c, d) = packedByteCount a + packedByteCount b + packedByteCount c + packedByteCount d
  {-# INLINE packedByteCount #-}
  packM (a, b, c, d) =
    packM a >> packM b >> packM c >> packM d
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    !c <- unpackM
    !d <- unpackM
    pure (a, b, c, d)
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b, MemPack c, MemPack d, MemPack e) => MemPack (a, b, c, d, e) where
  typeName =
    "("
      ++ intercalate
        ","
        [ typeName @a
        , typeName @b
        , typeName @c
        , typeName @d
        , typeName @e
        ]
      ++ ")"
  packedByteCount (a, b, c, d, e) =
    packedByteCount a + packedByteCount b + packedByteCount c + packedByteCount d + packedByteCount e
  {-# INLINE packedByteCount #-}
  packM (a, b, c, d, e) =
    packM a >> packM b >> packM c >> packM d >> packM e
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    !c <- unpackM
    !d <- unpackM
    !e <- unpackM
    pure (a, b, c, d, e)
  {-# INLINE unpackM #-}

instance (MemPack a, MemPack b, MemPack c, MemPack d, MemPack e, MemPack f) => MemPack (a, b, c, d, e, f) where
  typeName =
    "("
      ++ intercalate
        ","
        [ typeName @a
        , typeName @b
        , typeName @c
        , typeName @d
        , typeName @e
        , typeName @f
        ]
      ++ ")"
  packedByteCount (a, b, c, d, e, f) =
    packedByteCount a
      + packedByteCount b
      + packedByteCount c
      + packedByteCount d
      + packedByteCount e
      + packedByteCount f
  {-# INLINE packedByteCount #-}
  packM (a, b, c, d, e, f) =
    packM a >> packM b >> packM c >> packM d >> packM e >> packM f
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    !c <- unpackM
    !d <- unpackM
    !e <- unpackM
    !f <- unpackM
    pure (a, b, c, d, e, f)
  {-# INLINE unpackM #-}

instance
  (MemPack a, MemPack b, MemPack c, MemPack d, MemPack e, MemPack f, MemPack g) =>
  MemPack (a, b, c, d, e, f, g)
  where
  typeName =
    "("
      ++ intercalate
        ","
        [ typeName @a
        , typeName @b
        , typeName @c
        , typeName @d
        , typeName @e
        , typeName @f
        , typeName @g
        ]
      ++ ")"
  packedByteCount (a, b, c, d, e, f, g) =
    packedByteCount a
      + packedByteCount b
      + packedByteCount c
      + packedByteCount d
      + packedByteCount e
      + packedByteCount f
      + packedByteCount g
  {-# INLINE packedByteCount #-}
  packM (a, b, c, d, e, f, g) =
    packM a >> packM b >> packM c >> packM d >> packM e >> packM f >> packM g
  {-# INLINE packM #-}
  unpackM = do
    !a <- unpackM
    !b <- unpackM
    !c <- unpackM
    !d <- unpackM
    !e <- unpackM
    !f <- unpackM
    !g <- unpackM
    pure (a, b, c, d, e, f, g)
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

-- | This is a helper type useful for serializing number of elements in data
-- structures. It uses `VarLen` underneath, since sizes of common data structures aren't
-- too big. It also prevents negative values from being serialized and deserialized.
newtype Length = Length {unLength :: Int}
  deriving (Eq, Show, Num)

instance Bounded Length where
  minBound = 0
  maxBound = Length maxBound

instance Enum Length where
  toEnum n
    | n < 0 = error $ "toEnum: Length cannot be negative: " ++ show n
    | otherwise = Length n
  fromEnum = unLength

instance MemPack Length where
  packedByteCount = packedByteCount . VarLen . fromIntegral @Int @Word . unLength
  packM (Length n)
    | n < 0 = error $ "Length cannot be negative. Supplied: " ++ show n
    | otherwise = packM (VarLen (fromIntegral @Int @Word n))
  {-# INLINE packM #-}
  unpackM = do
    VarLen (w :: Word) <- unpackM
    when (testBit w (finiteBitSize w)) $
      F.fail $
        "Attempt to unpack negative length was detected: " ++ show (fromIntegral @Word @Int w)
    pure $ Length $ fromIntegral @Word @Int w
  {-# INLINE unpackM #-}

-- | This is a helper type that is useful for creating `MemPack` instances for sum types.
newtype Tag = Tag {unTag :: Word8}
  deriving (Eq, Ord, Show, Num, Enum, Bounded)

-- Manually defined instance, since ghc-8.6 has issues with deriving MemPack
instance MemPack Tag where
  packedByteCount _ = packedTagByteCount
  {-# INLINE packedByteCount #-}
  unpackM = unpackTagM
  {-# INLINE unpackM #-}
  packM = packTagM
  {-# INLINE packM #-}

packedTagByteCount :: Int
packedTagByteCount = SIZEOF_WORD8

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
