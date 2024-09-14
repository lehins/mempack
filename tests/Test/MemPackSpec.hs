{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.MemPack.Error
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

expectRoundTrip :: forall a. (MemPack a, Eq a, Show a) => a -> Expectation
expectRoundTrip a = do
  unpackError (pack a) `shouldBe` a
  unpackError (packByteString a) `shouldBe` a

memPackSpec :: forall a. (MemPack a, Arbitrary a, Eq a, Show a) => Spec
memPackSpec =
  describe (showType @a) $ do
    prop "RoundTrip" $ expectRoundTrip @a
    describe "Fail on empty" $ do
      let failOnEmpty emptyBuffer =
            case unpack emptyBuffer :: Either SomeError a of
              Left e
                | Just roob <- fromSomeError e -> do
                    ranOutOfBytesRead roob `shouldBe` 0
                    ranOutOfBytesAvailable roob `shouldBe` 0
                    ranOutOfBytesRequested roob `shouldSatisfy` (> 0)
                | otherwise -> expectationFailure $ "Unexpected failure: " ++ show e
              Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res
      it "ByteArray" $ failOnEmpty (mempty :: ByteArray)
      it "ByteString" $ failOnEmpty (mempty :: ByteString)
      it "ShortByteString" $ failOnEmpty (mempty :: ShortByteString)

spec :: Spec
spec = do
  memPackSpec @(E Int)
  memPackSpec @(E Word)
  memPackSpec @[E Int]
  memPackSpec @[E Word]
  memPackSpec @(E Int, E Word)
