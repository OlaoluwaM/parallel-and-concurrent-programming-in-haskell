module Concurrent.STM.Experiment where

import Control.Concurrent.STM
import Control.Concurrent.STM.Delay

main :: IO ()
main = do
    v <- newTVarIO "no side"
    delayA <- newDelay (10 ^ 5)
    delayB <- newDelay (10 ^ 8)

    val <- atomically $ do
        (waitDelay delayA >> writeTVar v "a side") `orElse` (waitDelay delayB >> writeTVar v "b side")
        readTVar v

    print val -- Will print "a side"

main2 :: IO ()
main2 = do
    v <- newTVarIO "no side"
    delayA <- newDelay (10 ^ 5)

    val <- atomically $ do
        (waitDelay delayA >> writeTVar v "a side") `orElse` writeTVar v "b side"
        readTVar v

    print val -- Will print "b side"
