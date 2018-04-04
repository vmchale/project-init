module Main (main) where

import           Criterion.Main
import           Lib

main :: IO ()
main =
    defaultMain [ bgroup "head"
                      [ bench "head" $ whnf head' [(1 :: Integer)..] ]
                ]
