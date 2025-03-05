module FibonacciServiceTest where

import Test.HUnit
import FibonacciService

-- Test cases for the basic Fibonacci function
testFibonacciBasic :: Test
testFibonacciBasic = TestList
  [ "fibonacci 0" ~: fibonacci 0 ~?= 0
  , "fibonacci 1" ~: fibonacci 1 ~?= 1
  , "fibonacci 2" ~: fibonacci 2 ~?= 1
  , "fibonacci 3" ~: fibonacci 3 ~?= 2
  , "fibonacci 4" ~: fibonacci 4 ~?= 3
  , "fibonacci 5" ~: fibonacci 5 ~?= 5
  , "fibonacci 6" ~: fibonacci 6 ~?= 8
  , "fibonacci 7" ~: fibonacci 7 ~?= 13
  ]

-- Test cases for the dynamic programming Fibonacci function
testFibonacciDP :: Test
testFibonacciDP = TestList
  [ "fibonacciDP 0" ~: fibonacciDP 0 ~?= 0
  , "fibonacciDP 1" ~: fibonacciDP 1 ~?= 1
  , "fibonacciDP 10" ~: fibonacciDP 10 ~?= 55
  , "fibonacciDP 20" ~: fibonacciDP 20 ~?= 6765
  ]

-- Test cases for the matrix-based Fibonacci function
testFibonacciMatrix :: Test
testFibonacciMatrix = TestList
  [ "fibonacciMatrix 0" ~: fibonacciMatrix 0 ~?= 0
  , "fibonacciMatrix 1" ~: fibonacciMatrix 1 ~?= 1
  , "fibonacciMatrix 10" ~: fibonacciMatrix 10 ~?= 55
  , "fibonacciMatrix 20" ~: fibonacciMatrix 20 ~?= 6765
  , "fibonacciMatrix 50" ~: fibonacciMatrix 50 ~?= 12586269025
  , "equivalence for n=30" ~: fibonacciMatrix 30 ~?= fibonacciDP 30
  ]

-- Test cases for isFibonacci function
testIsFibonacci :: Test
testIsFibonacci = TestList
  [ "isFibonacci 0" ~: isFibonacci 0 ~?= True
  , "isFibonacci 1" ~: isFibonacci 1 ~?= True
  , "isFibonacci 2" ~: isFibonacci 2 ~?= True
  , "isFibonacci 3" ~: isFibonacci 3 ~?= True
  , "isFibonacci 4" ~: isFibonacci 4 ~?= False
  , "isFibonacci 5" ~: isFibonacci 5 ~?= True
  , "isFibonacci 6" ~: isFibonacci 6 ~?= False
  , "isFibonacci 8" ~: isFibonacci 8 ~?= True
  , "isFibonacci 9" ~: isFibonacci 9 ~?= False
  , "isFibonacci 144" ~: isFibonacci 144 ~?= True
  ]

-- Test cases for fibonacciSequence function
testFibonacciSequence :: Test
testFibonacciSequence = TestList
  [ "fibonacciSequence 0" ~: fibonacciSequence 0 ~?= []
  , "fibonacciSequence 1" ~: fibonacciSequence 1 ~?= [0]
  , "fibonacciSequence 2" ~: fibonacciSequence 2 ~?= [0, 1]
  , "fibonacciSequence 8" ~: fibonacciSequence 8 ~?= [0, 1, 1, 2, 3, 5, 8, 13]
  ]

-- All tests
allTests :: Test
allTests = TestList
  [ testFibonacciBasic
  , testFibonacciDP
  , testFibonacciMatrix
  , testIsFibonacci
  , testFibonacciSequence
  ]

-- Main test runner
main :: IO Counts
main = runTestTT allTests
