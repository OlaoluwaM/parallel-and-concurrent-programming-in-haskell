module Concurrent.ThreadsAndMVars.MVar2 where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)

main :: IO ()
main = do
    m <- newEmptyMVar
    void $ forkIO $ putMVar m 'x' >> putMVar m 'y'
    r <- takeMVar m
    print r
    r' <- takeMVar m
    print r'
