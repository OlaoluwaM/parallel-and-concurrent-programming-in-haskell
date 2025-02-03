module Concurrent.Debugging.Deadlock2 where

import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, try)
import GHC.Conc (labelThread)

main :: IO ()
main = do
    t <- myThreadId
    labelThread t "main"
    lock <- newEmptyMVar
    child <- forkIO $ do
        r <- try (takeMVar lock)
        print (r :: Either SomeException ())
    labelThread child "child"
    threadDelay 1000000
    print (lock == lock)
