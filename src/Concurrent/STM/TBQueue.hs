module Concurrent.STM.TBQueue where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry)
import Control.Monad (when)

data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue capacityLimit = do
    readEnd <- newTVar []
    writeEnd <- newTVar []
    size <- newTVar capacityLimit
    pure $ TBQueue size readEnd writeEnd

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue availCapacityRef _ writeEndRef) val = do
  availCapacity <- readTVar availCapacityRef
  when (availCapacity == 0) retry
  writeEndL <- readTVar writeEndRef
  writeTVar availCapacityRef (availCapacity - 1)
  writeTVar writeEndRef (val:writeEndL)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue availCapacityRef readEndRef writeEndRef) = do
  availCapacity <- readTVar availCapacityRef
  writeTVar availCapacityRef (availCapacity + 1)
  readEndL <- readTVar readEndRef
  case readEndL of
    (x:xs) -> do
      writeTVar readEndRef xs
      pure x
    [] -> do
      writeEndL <- readTVar writeEndRef
      when (null writeEndL) retry
      let (y:ys) = reverse writeEndL
      writeTVar writeEndRef []
      writeTVar readEndRef ys
      pure y
