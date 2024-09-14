{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.MemPackSpec (spec) where

import Data.ByteString (ByteString)
import Data.MemPack
import Data.MemPack.Error
import Test.Common

expectRoundTrip :: forall a. (MemPack a, Eq a, Show a) => a -> Expectation
expectRoundTrip a = do
  unpackError (pack a) `shouldBe` a
  unpackError (packByteString a) `shouldBe` a

memPackSpec :: forall a. (MemPack a, Arbitrary a, Eq a, Show a) => Spec
memPackSpec =
  describe (showType @a) $ do
    prop "RoundTrip" $ expectRoundTrip @a
    it "Fail on empty" $ do
      case unpack (mempty :: ByteString) :: Either SomeError a of
        Left _ -> pure ()
        Right res -> expectationFailure $ "Unexpectedly unpacked: " ++ show res

spec :: Spec
spec = do
  memPackSpec @Int
  memPackSpec @Word
  memPackSpec @[Int]
  memPackSpec @[Word]
  memPackSpec @(Int, Word)
