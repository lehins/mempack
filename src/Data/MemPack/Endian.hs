{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.MemPack.Endian (
  BE (..),
  LE (..),
  NativeEndian,
  pattern NativeEndian,
  nativeEndian,
  ForeignEndian,
  pattern ForeignEndian,
  foreignEndian,

  -- * Helpers
  byteSwap,
  wordSizeInBits,
) where

import Data.Bits
import Data.Int
import Data.MemPack
import Data.Word
import GHC.ByteOrder
import GHC.Exts
#include "MachDeps.h"
#include "HsBaseConfig.h"

-- | Size of the machine word in number of bits.
wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)

newtype LE a = LE a
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype BE a = BE a
  deriving (Eq, Ord, Show, Enum, Bounded)

byteSwap :: Word -> Word
byteSwap (W# w#) = W# (byteSwap# w#)
{-# INLINE byteSwap #-}

#ifdef WORDS_BIGENDIAN
nativeEndian :: ByteOrder
nativeEndian = BigEndian

foreignEndian :: ByteOrder
foreignEndian = LittleEndian

type ForeignEndian = LE
pattern ForeignEndian :: a -> ForeignEndian a
pattern ForeignEndian a = LE a
{-# COMPLETE ForeignEndian #-}

type NativeEndian = BE
pattern NativeEndian :: a -> NativeEndian a
pattern NativeEndian a = BE a
{-# COMPLETE NativeEndian #-}

#else
nativeEndian :: ByteOrder
nativeEndian = LittleEndian

foreignEndian :: ByteOrder
foreignEndian = BigEndian

type ForeignEndian = BE
pattern ForeignEndian :: a -> ForeignEndian a
pattern ForeignEndian a = BE a
{-# COMPLETE ForeignEndian #-}

type NativeEndian = LE
pattern NativeEndian :: a -> NativeEndian a
pattern NativeEndian a = LE a
{-# COMPLETE NativeEndian #-}

#endif

-- One byte or less size types.

deriving instance MemPack (BE Word8)
deriving instance MemPack (BE Int8)
deriving instance MemPack (BE Bool)
deriving instance MemPack (BE ())

deriving instance MemPack (LE Word8)
deriving instance MemPack (LE Int8)
deriving instance MemPack (LE Bool)
deriving instance MemPack (LE ())

-- Native deriving

deriving instance MemPack (NativeEndian Char)
deriving instance MemPack (NativeEndian Word)
deriving instance MemPack (NativeEndian Word16)
deriving instance MemPack (NativeEndian Word32)
deriving instance MemPack (NativeEndian Word64)
deriving instance MemPack (NativeEndian Int)
deriving instance MemPack (NativeEndian Int16)
deriving instance MemPack (NativeEndian Int32)
deriving instance MemPack (NativeEndian Int64)
deriving instance MemPack (NativeEndian Float)
deriving instance MemPack (NativeEndian Double)
