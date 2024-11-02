module Concurrent.ThreadsAndMVars.MVar3 where

import Control.Concurrent (newEmptyMVar, takeMVar)

main :: IO ()
main = do
    m <- newEmptyMVar
    takeMVar m
