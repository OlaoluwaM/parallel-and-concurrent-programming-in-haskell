module Concurrent.Debugging.ThreadPerf2 where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, void)

numThreads :: Int
numThreads = 1000000

main :: IO ()
main = do
    ms <- replicateM numThreads $ do
        m <- newEmptyMVar
        void $ forkIO $ putMVar m ()
        pure m
    mapM_ takeMVar ms
