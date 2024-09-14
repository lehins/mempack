{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
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

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Fail
import Data.Array.Byte
import Data.Bifunctor (first)
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

newtype Pack s a = Pack
  { runPack :: StateT Int (ST s) a
  }
  deriving (Functor, Applicative, Monad, MonadState Int)

newtype Unpack a = Unpack
  { runUnpack :: StateT Int (Fail SomeError) a
  }
  deriving (Functor, Applicative, Monad, MonadFail, MonadState Int)

failUnpack :: Error e => e -> Unpack a
failUnpack = Unpack . lift . failT . toSomeError

class MemPack a where
  showType :: String
  default showType :: Typeable a => String
  showType = show (typeRep (Proxy @a))

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

instance (MemPack a, MemPack b) => MemPack (a, b) where
  showType = "(" ++ showType @a ++ "," ++ showType @b ++ ")"
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
  showType = "[" ++ showType @a ++ "]"
  packedByteCount es = packedByteCount (length es) + getSum (foldMap (Sum . packedByteCount) es)
  {-# INLINE packedByteCount #-}
  unsafePackInto mba as = do
    unsafePackInto mba (length as)
    mapM_ (unsafePackInto mba) as
  {-# INLINE unsafePackInto #-}
  unpackBuffer buf = do
    n <- unpackBuffer buf
    replicateM n (unpackBuffer buf)
  {-# INLINE unpackBuffer #-}

lift_# :: (State# s -> State# s) -> Pack s ()
lift_# f = Pack $ lift $ ST $ \s# -> (# f s#, () #)
{-# INLINE lift_# #-}

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
packByteArray isPinned a = runST $ do
  MutableByteArray mba# <- packMutableByteArray isPinned a
  ST $ \s# -> case unsafeFreezeByteArray# mba# s# of
    (# s'#, ba# #) -> (# s'#, ByteArray ba# #)
{-# INLINE packByteArray #-}

packMutableByteArray ::
  forall a s. (MemPack a, HasCallStack) => Bool -> a -> ST s (MutableByteArray s)
packMutableByteArray isPinned a = do
  let !len@(I# len#) = packedByteCount a
  mba <-
    ST $ \s# -> case (if isPinned then newPinnedByteArray# else newByteArray#) len# s# of
      (# s'#, mba# #) -> (# s'#, MutableByteArray mba# #)
  filledBytes <- execStateT (runPack (unsafePackInto mba a)) 0
  when (filledBytes /= len) $
    if (filledBytes < len)
      then
        error $
          "Some bug in 'unsafePackInto' was detected. Buffer of length " <> showBytes len
            ++ " was not fully filled while packing " <> showType @a
            ++ ". Unfilled " <> showBytes (len - filledBytes) <> "."
      else
        -- This is a critical error, therefore we are not gracefully failing this unpacking
        error $
          "Potential buffer overflow. Some bug in 'unsafePackInto' was detected while packing " <> showType @a
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
      "Potential buffer overflow. Some bug in 'unpackBuffer' was detected while unpacking " <> showType @a
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
          { notFullyConsumedRead = len
          , notFullyConsumedAvailable = consumedBytes
          , notFullyConsumedTypeName = showType @a
          }
  pure a
{-# INLINEABLE unpackFail #-}

unpackError :: forall a b. (MemPack a, Buffer b, HasCallStack) => b -> a
unpackError = errorFail . unpackFail
{-# INLINE unpackError #-}

newtype VarLen a = VarLen a
