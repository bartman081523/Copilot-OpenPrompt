module Main where

import Criterion.Main
import qualified FibonacciService as F
import Control.DeepSeq

-- | Define benchmarks for different Fibonacci implementations
main :: IO ()
main = defaultMain [
    bgroup "Fibonacci implementations" [
        bgroup "n=10" [
            bench "naive" $ whnf F.fibonacci 10,
            bench "dynamic programming" $ whnf F.fibonacciDP 10,
            bench "matrix" $ whnf F.fibonacciMatrix 10
        ],
        bgroup "n=20" [
            bench "dynamic programming" $ whnf F.fibonacciDP 20,
            bench "matrix" $ whnf F.fibonacciMatrix 20
        ],
        bgroup "n=30" [
            bench "dynamic programming" $ whnf F.fibonacciDP 30,
            bench "matrix" $ whnf F.fibonacciMatrix 30
        ],
        bgroup "n=40" [
            -- Skipping naive for large inputs as it's too slow
            bench "dynamic programming" $ whnf F.fibonacciDP 40,
            bench "matrix" $ whnf F.fibonacciMatrix 40
        ],
        bgroup "n=1000" [
            bench "matrix" $ whnf F.fibonacciMatrix 1000
        ]
    ],
    bgroup "Utility functions" [
        bench "isFibonacci 144" $ whnf F.isFibonacci 144,
        bench "isFibonacci 143" $ whnf F.isFibonacci 143,
        bench "fibonacciSequence 30" $ nf F.fibonacciSequence 30
    ]
  ]
