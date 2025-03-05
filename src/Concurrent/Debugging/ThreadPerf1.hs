module Concurrent.Debugging.ThreadPerf1 where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_)

numThreads :: Int
numThreads = 1000000

main :: IO ()
main = do
    m <- newEmptyMVar
    replicateM_ numThreads $ forkIO (putMVar m ())
    replicateM_ numThreads $ takeMVar m
