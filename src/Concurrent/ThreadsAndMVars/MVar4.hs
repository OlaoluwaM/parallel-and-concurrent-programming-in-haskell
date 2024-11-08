module Concurrent.ThreadsAndMVars.MVar4 where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar)
import Control.Monad (void)

main :: IO ()
main = do
    m <- newEmptyMVar
    void $ forkIO $ print "running in other thread" >> takeMVar m >>= print @String
    print "Main thread done"
