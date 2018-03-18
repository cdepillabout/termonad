
module Main where

import Criterion.Main (bench, bgroup, defaultMain, nf)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "example1"
        [ bench "plus" $ nf (1 +) (1 :: Int)
        , bench "minus" $ nf (1 -) (1 :: Int)
        ]
    ]
