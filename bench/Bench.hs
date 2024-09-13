module Main where

import Criterion.Main

main :: IO ()
main = do
  defaultMain
    [ bgroup "Bench"
      [ bench "someFunc" $ nfIO (pure ())
      ]
    ]
