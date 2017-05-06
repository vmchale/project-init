module Main where

import Criterion.Main

main = do
    defaultMain [ bgroup "head"
                      [ bench "fortune-teller" $ whnf head [1..] ]
                ]
