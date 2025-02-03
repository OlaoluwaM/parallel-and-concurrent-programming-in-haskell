module Concurrent.Debugging.MVar4 where

import Control.Concurrent
    ( forkIO, myThreadId, newEmptyMVar, putMVar, takeMVar )
import GHC.Conc ( labelThread )
import Debug.Trace ( traceEventIO )

main :: IO Char
main = do
  t <- myThreadId
  labelThread t "main"
  m <- newEmptyMVar
  t <- forkIO $ putMVar m 'a'
  labelThread t "a"
  t <- forkIO $ putMVar m 'b'
  labelThread t "b"
  traceEventIO "before takeMVar"
  takeMVar m
  takeMVar m
