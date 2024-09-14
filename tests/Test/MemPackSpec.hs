{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.MemPackSpec (spec) where

import Data.MemPack
import Test.Common

expectRoundTrip :: forall a. (MemPack a, Eq a, Show a) => a -> Expectation
expectRoundTrip a = do
  unpackError (pack a) `shouldBe` a
  unpackError (packByteString a) `shouldBe` a

propRoundTrip :: forall a. (MemPack a, Arbitrary a, Eq a, Show a) => Spec
propRoundTrip =
  prop (showType @a) $ expectRoundTrip @a

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    propRoundTrip @Int
