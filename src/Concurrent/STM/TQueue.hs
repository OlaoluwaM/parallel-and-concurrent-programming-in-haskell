{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Concurrent.STM.TQueue where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, retry, writeTVar)
import Control.Monad (when)

data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
    readEnd <- newTVar []
    writeEnd <- newTVar []
    pure $ TQueue readEnd writeEnd

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _ writeEndRef) val = do
    writeEndL <- readTVar writeEndRef
    writeTVar writeEndRef (val : writeEndL)

readTQueue :: TQueue a -> STM a
readTQueue (TQueue readEndRef writeEndRef) = do
    readEndL <- readTVar readEndRef
    case readEndL of
        (x : xs) -> writeTVar readEndRef xs >> pure x
        [] -> do
            writeEndL <- readTVar writeEndRef
            when (null writeEndL) retry
            let (y : ys) = reverse writeEndL
            writeTVar writeEndRef []
            writeTVar readEndRef ys
            pure y
