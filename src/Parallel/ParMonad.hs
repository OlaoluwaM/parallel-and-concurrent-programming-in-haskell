module Parallel.ParMonad where

import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace
-- NB. using Trace here, Direct is too strict and forces the fibs in
-- the parent; see https://github.com/simonmar/monad-par/issues/27

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

main = do
  args <- getArgs
  let [n,m] = map read args
  print $
-- <<runPar
    runPar $ do
      i <- new                          -- <1>
      j <- new                          -- <2>
      fork (put i (fib n))              -- <3> fib n will be evaluated in parallel
      fork (put j (fib m))              -- <4> fib m will be evaluated in parallel
      a <- get i                        -- <5>
      b <- get j                        -- <6>
      pure (a+b)                      -- <7>
-- >>
