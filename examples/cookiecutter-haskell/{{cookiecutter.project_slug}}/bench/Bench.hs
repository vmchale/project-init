module Main where

import Criterion.Main

main = do
    defaultMain [ bgroup "infinity"
                      [ bench "head" $ whnf head [1..] ]
                ]
