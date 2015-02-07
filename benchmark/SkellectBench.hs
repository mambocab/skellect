module SkellectBench (benchmarks) where

import Criterion (bench, nf, Benchmark)
import Skellect (skellect)

benchmarks :: [Benchmark]
benchmarks = 
    [ bench "skellect" (nf (const skellect) ())
    ]

