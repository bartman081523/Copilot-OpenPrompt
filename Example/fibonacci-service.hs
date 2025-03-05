{-|
Module      : FibonacciService
Description : A service for calculating Fibonacci numbers and related operations
Copyright   : (c) 2023
License     : MIT
Maintainer  : example@example.com

This module provides functions for calculating Fibonacci numbers using
different algorithms, as well as related operations like checking if
a number is a Fibonacci number.
-}

module FibonacciService 
  ( -- * Fibonacci calculation functions
    fibonacci
  , fibonacciDP
  , fibonacciMatrix
    -- * Utility functions
  , isFibonacci
  , fibonacciSequence
    -- * Entry point
  , main
  ) where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))

-- | Calculate the nth Fibonacci number using a naive recursive approach.
-- Warning: This implementation is very slow for large inputs due to exponential complexity.
-- Time complexity: O(2^n)
-- Space complexity: O(n) for the call stack
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- | Calculate the nth Fibonacci number using dynamic programming (memoization).
-- This is much more efficient than the naive recursive approach.
-- Time complexity: O(n)
-- Space complexity: O(n)
fibonacciDP :: Integer -> Integer
fibonacciDP n = go n Map.empty
  where
    go :: Integer -> Map.Map Integer Integer -> Integer
    go 0 _ = 0
    go 1 _ = 1
    go i memo =
      case Map.lookup i memo of
        Just val -> val
        Nothing ->
          let val1 = go (i - 1) memo
              val2 = go (i - 2) (Map.insert (i - 1) val1 memo)
              result = val1 + val2
          in result

-- | Calculate the nth Fibonacci number using matrix exponentiation.
-- This is the most efficient implementation for large values of n.
-- Time complexity: O(log n)
-- Space complexity: O(1)
fibonacciMatrix :: Integer -> Integer
fibonacciMatrix n
  | n == 0    = 0
  | otherwise = fst $ power (1, 1, 1, 0) n
  where
    -- Matrix multiplication for a 2x2 matrix represented as (a,b,c,d)
    multiply (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)

    -- Fast matrix exponentiation by squaring
    power matrix 1 = matrix
    power matrix n
      | even n    = power (multiply matrix matrix) (n `div` 2)
      | otherwise = multiply matrix $ power (multiply matrix matrix) (n `div` 2)

-- | Check if a given number is a Fibonacci number.
-- Uses the property that a number is Fibonacci if and only if
-- 5n² + 4 or 5n² - 4 is a perfect square.
-- Time complexity: O(1)
isFibonacci :: Integer -> Bool
isFibonacci n = isPerfectSquare (5 * n * n + 4) || isPerfectSquare (5 * n * n - 4)
  where
    isPerfectSquare x = sq * sq == x
      where sq = floor . sqrt $ fromIntegral x

-- | Generate a list of the first n Fibonacci numbers
-- Time complexity: O(n)
-- Space complexity: O(n)
fibonacciSequence :: Integer -> [Integer]
fibonacciSequence n
  | n <= 0    = []
  | otherwise = map fibonacciMatrix [0..(n-1)]

-- | Command line options
data Options = Options
  { optAlgorithm :: String
  , optVerbose   :: Bool
  , optCheck     :: Maybe Integer
  , optSequence  :: Bool
  }

-- | Default options
defaultOptions :: Options
defaultOptions = Options
  { optAlgorithm = "matrix"
  , optVerbose   = False
  , optCheck     = Nothing
  , optSequence  = False
  }

-- | Option descriptors
options :: [OptDescr (Options -> Options)]
options =
  [ Option "a" ["algorithm"]
      (ReqArg (\alg opts -> opts { optAlgorithm = alg }) "ALGORITHM")
      "Specify algorithm: naive, dp, matrix (default: matrix)"
  , Option "v" ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Enable verbose output"
  , Option "c" ["check"]
      (ReqArg (\n opts -> opts { optCheck = readMaybe n }) "NUMBER")
      "Check if a number is a Fibonacci number"
  , Option "s" ["sequence"]
      (NoArg (\opts -> opts { optSequence = True }))
      "Print a sequence of Fibonacci numbers"
  , Option "h" ["help"]
      (NoArg (\_ -> error "Help requested"))
      "Show this help text"
  ]

-- | Parse command line arguments
parseArgs :: [String] -> IO (Options, [String])
parseArgs args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: fibonacci-service [OPTION...] [number]"

-- | Main function to run the program
main :: IO ()
main = do
  args <- getArgs
  (opts, nonOpts) <- parseArgs args
  
  -- Handle help request
  when (optVerbose opts) $ do
    putStrLn "Fibonacci Number Calculator"
    putStrLn $ "Using algorithm: " ++ optAlgorithm opts
  
  -- Process based on options
  case nonOpts of
    [numStr] -> processNumber opts numStr
    [] -> processOptions opts
    _ -> putStrLn $ "Usage: fibonacci-service [OPTION...] [number]\nTry 'fibonacci-service --help' for more information."

-- | Process a number based on options
processNumber :: Options -> String -> IO ()
processNumber opts numStr =
  case readMaybe numStr :: Maybe Integer of
    Just n
      | n >= 0 -> do
          let fibFunc = selectAlgorithm (optAlgorithm opts)
          let result = fibFunc n
          putStrLn $ "The " ++ show n ++ "th Fibonacci number is: " ++ show result
          when (optVerbose opts) $
            putStrLn $ "Calculation performed using the " ++ optAlgorithm opts ++ " algorithm."
      | otherwise -> putStrLn "Please provide a non-negative integer"
    Nothing -> putStrLn "Please provide a valid integer"

-- | Process options without a specific number argument
processOptions :: Options -> IO ()
processOptions opts
  | optSequence opts = do
      putStrLn "Enter the number of Fibonacci numbers to generate:"
      numStr <- getLine
      case readMaybe numStr :: Maybe Integer of
        Just n
          | n >= 0 -> do
              let seq = fibonacciSequence n
              putStrLn $ "Fibonacci sequence: " ++ show seq
          | otherwise -> putStrLn "Please provide a non-negative integer"
        Nothing -> putStrLn "Please provide a valid integer"
  | Just num <- optCheck opts = do
      let isF = isFibonacci num
      putStrLn $ show num ++ (if isF then " is" else " is not") ++ " a Fibonacci number."
  | otherwise -> do
      putStrLn $ "Usage: fibonacci-service [OPTION...] [number]\nTry 'fibonacci-service --help' for more information."
      exitWith (ExitFailure 1)

-- | Select the appropriate Fibonacci calculation function based on the algorithm name
selectAlgorithm :: String -> (Integer -> Integer)
selectAlgorithm alg = case alg of
  "naive"  -> fibonacci
  "dp"     -> fibonacciDP
  "matrix" -> fibonacciMatrix
  _        -> fibonacciMatrix -- Default to the most efficient algorithm
