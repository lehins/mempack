{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MemPackSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Fail
import Data.Array.Byte (ByteArray(..))
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal as SBS (ShortByteString(..))
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Word
import System.Random.Stateful
import Test.Common

-- | Generate extrema around boundaries
newtype E a = E {unE :: a}
  deriving (Eq, Show, MemPack)

instance (Arbitrary a, Bounded a, Num a, Random a) => Arbitrary (E a) where
  arbitrary =
    E
      <$> frequency
        [ (25, arbitrary)
        , (25, chooseAny)
        , (25, choose (minBound, minBound + 100))
        , (25, choose (maxBound, maxBound - 100))
        ]

deriving instance Random a => Random (VarLen a)
deriving instance Arbitrary a => Arbitrary (VarLen a)

data Backtrack
  = IntCase Int
  | Word16Case Word16
  deriving (Show, Eq)

instance Arbitrary Backtrack where
  arbitrary =
    oneof [IntCase . unE <$> arbitrary, Word16Case . unE <$> arbitrary]

instance MemPack Backtrack where
  packedByteCount =
    (+ 1) . \case
      IntCase i -> packedByteCount i
      Word16Case i -> packedByteCount i
  unsafePackInto buf = \case
    IntCase i -> unsafePackInto buf (0 :: Word8) >> unsafePackInto buf i
    Word16Case i -> unsafePackInto buf (1 :: Word8) >> unsafePackInto buf i
  unpackBuffer buf =
    (IntCase <$> unpackCase 0) <|> (Word16Case <$> unpackCase 1)
    where
      unpackCase :: MemPack a => Word8 -> Unpack a
      unpackCase t = do
        t' <- unpackBuffer buf
        unless (t == t') $ fail "Tag mismatch"
        unpackBuffer buf

expectRoundTrip :: forall a. (MemPack a, Eq a, Show a) => a -> Expectation
expectRoundTrip a = do
  unpackError (pack a) `shouldBe` a
  unpackError (packByteString a) `shouldBe` a

expectNotFullyConsumed ::
  forall a. (MemPack a, Show a) => a -> NonEmptyList Word8 -> Expectation
expectNotFullyConsumed a (NonEmpty xs) = do
  let extraByteCount = length xs
      failOnExtra :: (Buffer b, Semigroup b) => b -> b -> Expectation
      failOnExtra buf extra =
        case unpack (buf <> extra) :: Either SomeError a of
          Left e
            | Just err <- fromSomeError e -> do
                notFullyConsumedRead err `shouldBe` bufferByteCount buf
                notFullyConsumedAvailable err `shouldBe` bufferByteCount buf
                  + packedByteCount (Length extraByteCount) -- account for list length
                  + extraByteCount
                notFullyConsumedTypeName err `shouldBe` typeName @a
            | otherwise -> expectationFailure $ "Unexpected failure: " ++ show e
          Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res
  failOnExtra (pack a) (pack xs)
  failOnExtra (packByteString a) (packByteString xs)

memPackSpec :: forall a. (MemPack a, Arbitrary a, Eq a, Show a) => Spec
memPackSpec =
  describe (typeName @a) $ do
    prop "RoundTrip" $ expectRoundTrip @a
    describe "Fail on empty" $ do
      let checkRanOutOfBytes someErr
            | Just err <- fromSomeError someErr = do
                ranOutOfBytesRead err `shouldBe` 0
                ranOutOfBytesAvailable err `shouldBe` 0
                ranOutOfBytesRequested err `shouldSatisfy` (> 0)
            | otherwise = expectationFailure $ "Unexpected failure: " ++ show someErr
          -- Check that the only kind of failure we get is RanOutOfBytes
          failOnEmpty emptyBuffer =
            case unpack emptyBuffer :: Either SomeError a of
              Left e
                | Just (ManyErrors errs) <- fromSomeError e -> mapM_ checkRanOutOfBytes errs
                | otherwise -> checkRanOutOfBytes e
              Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res
      it "ByteArray" $ failOnEmpty (mempty :: ByteArray)
      it "ByteString" $ failOnEmpty (mempty :: ByteString)
      it "ShortByteString" $ failOnEmpty (mempty :: ShortByteString)
    prop "Fail on too much" $ expectNotFullyConsumed @a

spec :: Spec
spec = do
  memPackSpec @(E Int)
  memPackSpec @(E Word)
  memPackSpec @(E Word8)
  memPackSpec @(E Word16)
  memPackSpec @(E Word32)
  memPackSpec @(E Word64)
  memPackSpec @[E Int]
  memPackSpec @[E Word]
  memPackSpec @(E Int, E Word)
  memPackSpec @(E (VarLen Word))
  memPackSpec @(E (VarLen Word16))
  memPackSpec @(E (VarLen Word32))
  memPackSpec @(E (VarLen Word64))
  memPackSpec @ByteArray
  memPackSpec @ShortByteString
  memPackSpec @Backtrack



