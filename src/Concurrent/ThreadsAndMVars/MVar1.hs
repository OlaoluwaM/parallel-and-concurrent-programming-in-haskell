module Concurrent.ThreadsAndMVars.MVar1 where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)

main :: IO ()
main = do
    m <- newEmptyMVar
    void $ forkIO $ putMVar m 'x'
    r <- takeMVar m
    print r
