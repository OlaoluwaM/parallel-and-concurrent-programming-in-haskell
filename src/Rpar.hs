module Rpar where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Data.List.Extra ((!?))
import Control.Monad (void, unless)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

-- <<main
main :: IO ()
main = do
  [n] <- getArgs
  let ind = readMaybe @Int n
  unless (isJust ind) $ fail $ "Invalid argument. Expected integer between 1 and 4, but received " <> show n
  let testIndex = fromJust ind - 1
  let testMaybe = [test1,test2,test3,test4] !? testIndex
  unless (isJust testMaybe) $ fail $ "No such test at index " <> show testIndex
  test <- fromJust testMaybe
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
-- >>

type Test = IO (Eval (Integer, Integer))

-- <<test1
test1 :: Test
test1 = do
  print "Test 1: rpar/rpar"
  pure $ do
    x <- rpar (fib 36)
    y <- rpar (fib 35)
    pure (x,y)
-- >>

-- <<test2
test2 :: Test
test2 = do
  print "Test 2: rpar/rseq"
  pure $ do
    x <- rpar (fib 36)
    y <- rseq (fib 35)
    pure (x,y)
-- >>

-- <<test3
test3 :: Test
test3 = do
  print "Test 3: rpar/rseq/rseq"
  pure $ do
    x <- rpar (fib 36)
    y <- rseq (fib 35)
    void . rseq $ x
    pure (x,y)
-- >>

-- <<test4
test4 :: Test
test4 = do
  print "Test 4: rpar/rpar/rseq"
  pure $ do
    x <- rpar (fib 36)
    y <- rpar (fib 35)
    rseq (x,y)
-- >>

printTimeSince :: UTCTime -> IO ()
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
