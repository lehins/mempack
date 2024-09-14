{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.MemPackSpec (spec) where

import Control.Monad.State.Strict
import Control.Monad.Trans.Fail
import Data.Array.Byte (ByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Word
import System.Random
import Test.Common

-- | Generate extrema around boundaries
newtype E a = E a
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
                  + packedByteCount extraByteCount -- account for list length
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
      let failOnEmpty emptyBuffer =
            case unpack emptyBuffer :: Either SomeError a of
              Left e
                | Just err <- fromSomeError e -> do
                    ranOutOfBytesRead err `shouldBe` 0
                    ranOutOfBytesAvailable err `shouldBe` 0
                    ranOutOfBytesRequested err `shouldSatisfy` (> 0)
                | otherwise -> expectationFailure $ "Unexpected failure: " ++ show e
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
  memPackSpec @[E Int]
  memPackSpec @[E Word]
  memPackSpec @(E Int, E Word)
  memPackSpec @(E (VarLen Word16))
