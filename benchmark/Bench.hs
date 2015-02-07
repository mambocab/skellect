module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import SkellectBench

main :: IO ()
main = defaultMain
    [ bgroup "Skellect" SkellectBench.benchmarks
    ]

