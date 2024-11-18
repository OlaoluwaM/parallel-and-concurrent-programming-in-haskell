module Concurrent.STM.AsyncSTM where

import Concurrent.STM.Blocking (TMVar, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.STM (STM, atomically, orElse, throwSTM, retry)
import Control.Exception (SomeException)

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    t <- forkFinally action (atomically . putTMVar var)
    pure $ Async t var

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
    r <- waitCatchSTM a
    either throwSTM pure r

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $ (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

waitAny :: [Async a] -> IO a
waitAny = atomically . foldr (orElse . waitSTM) retry


