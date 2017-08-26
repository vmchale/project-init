module Main where

import Criterion.Main

main = do
    defaultMain [ bgroup "head"
                      [ bench "head" $ whnf head [1..] ]
                ]
