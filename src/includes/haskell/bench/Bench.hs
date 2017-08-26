module Main where

import Criterion.Main

main :: IO ()
main =
    defaultMain [ bgroup "head"
                      [ bench "head" $ whnf head [1..] ]
                ]
