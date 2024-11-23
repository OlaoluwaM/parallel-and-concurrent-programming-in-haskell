module Concurrent.STM.TList where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, retry, writeTVar)

-- An alternate channel implementation, though it seems like this approach cannot have a dupChan operation
-- we can do this because an STM operation can block on any arbitrary condition. From this we gain the flexibility to choose whatever data structure we desire to model our channel
newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
    v <- newTVar []
    pure (TList v)

writeTList :: TList a -> a -> STM ()
writeTList (TList v) a = do
    list <- readTVar v
    writeTVar v (list ++ [a])

readTList :: TList a -> STM a
readTList (TList v) = do
    xs <- readTVar v
    case xs of
        [] -> retry
        (x : xs') -> do
            writeTVar v xs'
            pure x

-- TODO: Implement unGetChan and isEmptyChan
