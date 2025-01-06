{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Common (
  module X,
  QC (..),
) where

import Data.Array.Byte (ByteArray (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal as SBS (ShortByteString (..))
import Data.MemPack.Buffer (byteArrayFromShortByteString)
import qualified Data.Text as T
import System.Random.Stateful
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck as X
import Test.QuickCheck.Gen (Gen (MkGen))

data QC = QC

instance StatefulGen QC Gen where
  uniformWord32 QC = MkGen (\r _n -> runStateGen_ r uniformWord32)
  {-# INLINE uniformWord32 #-}
  uniformWord64 QC = MkGen (\r _n -> runStateGen_ r uniformWord64)
  {-# INLINE uniformWord64 #-}
  uniformShortByteString k QC =
    MkGen (\r _n -> runStateGen_ r (uniformShortByteString k))
  {-# INLINE uniformShortByteString #-}

instance Arbitrary ByteArray where
  arbitrary = qcByteArray . getNonNegative =<< arbitrary

instance Arbitrary ByteString where
  arbitrary = qcByteString . getNonNegative =<< arbitrary

instance Arbitrary SBS.ShortByteString where
  arbitrary = qcShortByteString . getNonNegative =<< arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

qcByteArray :: Int -> Gen ByteArray
qcByteArray n = byteArrayFromShortByteString <$> qcShortByteString n

qcByteString :: Int -> Gen ByteString
qcByteString n = uniformByteStringM (fromIntegral n) QC

qcShortByteString :: Int -> Gen SBS.ShortByteString
qcShortByteString n = uniformShortByteString (fromIntegral n) QC
