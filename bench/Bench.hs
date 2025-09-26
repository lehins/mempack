{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Codec.Serialise as Serialise
import Criterion.Main
import qualified Data.Avro as Avro
import qualified Data.Binary as Binary
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (fromShort, toShort)
import Data.MemPack
import Data.Primitive.Array (Array)
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import qualified Flat as Flat
import GHC.Exts (fromList)

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "packedByteCount"
        [ env (pure [1 :: Integer .. 1000000]) $ bench "[Integer]" . whnf packedByteCount
        ]
    , bgroup "[Int]" [env (pure [1 :: Int .. 100000]) packBench]
    , env (pure $ fromList @(Array Int) [1 :: Int .. 100000]) $ \arr ->
        bgroup
          "Array Int"
          [ bench "Pack" $ nf packByteString arr
          , env (pure (packByteString arr)) (bench "Unpack" . nf (unpackError @(Array Int)))
          , bench "RoundTrip" $ nf (unpackError @(Array Int) . packShortByteString) arr
          ]
    , env (pure [1 :: Int .. 100000]) $ \xs ->
        bgroup
          "RoundTrip List Through"
          [ bgroup
              "ByteString"
              [ bench "MemPack" $ nf (unpackError @[Int] . packByteString) xs
              , bench "Store" $ nf (Store.decodeEx @[Int] . Store.encode) xs
              , bench "Flat" $ nf (either (error . show) id . Flat.unflat @[Int] . Flat.flat) xs
              , bench "Cereal" $ nf (either (error . show) id . Cereal.decode @[Int] . Cereal.encode) xs
              , bench "Binary" $ nf (Binary.decode @[Int] . fromStrict . toStrict . Binary.encode) xs
              , bench "Serialise" $
                  nf (Serialise.deserialiseOrFail @[Int] . fromStrict . toStrict . Serialise.serialise) xs
              , bench "Avro" $
                  nf (Avro.decodeValue @[Int] . fromStrict . toStrict . Avro.encodeValue) xs
              ]
          , bgroup
              "ShortByteString"
              [ bench "MemPack" $ nf (unpackError @[Int] . packShortByteString) xs
              , bench "Store" $ nf (Store.decodeEx @[Int] . fromShort . toShort . Store.encode) xs
              , bench "Flat" $
                  nf (either (error . show) id . Flat.unflat @[Int] . fromShort . toShort . Flat.flat) xs
              , bench "Cereal" $
                  nf (either (error . show) id . Cereal.decode @[Int] . fromShort . toShort . Cereal.encode) xs
              , bench "Binary" $
                  nf
                    ( Binary.decode @[Int]
                        . fromStrict
                        . fromShort
                        . toShort
                        . toStrict
                        . Binary.encode
                    )
                    xs
              , bench "Serialise" $
                  nf
                    ( Serialise.deserialiseOrFail @[Int]
                        . fromStrict
                        . fromShort
                        . toShort
                        . toStrict
                        . Serialise.serialise
                    )
                    xs
              ]
          ]
    ]

packBench ::
  ( MemPack a
  , Store.Store a
  , Flat.Flat a
  , Cereal.Serialize a
  , Binary.Binary a
  , Serialise.Serialise a
  , Avro.HasAvroSchema a
  , Avro.ToAvro a
  ) =>
  a -> Benchmark
packBench xs =
  bgroup
    "Pack"
    [ bgroup
        "ByteString"
        [ bench "MemPack" $ nf packByteString xs
        , bench "Store" $ nf Store.encode xs
        , bench "Flat" $ nf Flat.flat xs
        , bench "Cereal" $ nf Cereal.encode xs
        , bench "Binary" $ nf (toStrict . Binary.encode) xs
        , bench "Serialise" $ nf (toStrict . Serialise.serialise) xs
        , bench "Avro" $ nf (toStrict . Avro.encodeValue) xs
        ]
    ]
