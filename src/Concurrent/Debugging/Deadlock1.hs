module Concurrent.Debugging.Deadlock1 where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar, putMVar, myThreadId)
import Control.Exception (finally)
import GHC.Conc (labelThread)

main :: IO ()
main = do
  t <- myThreadId
  labelThread t "main"
  lock <- newEmptyMVar
  complete <- newEmptyMVar
  child <- forkIO $ takeMVar lock `finally` putMVar complete ()
  labelThread child "child"
  takeMVar complete
