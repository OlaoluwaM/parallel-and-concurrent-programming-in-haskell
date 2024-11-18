module Concurrent.STM.BlockingRetry where

import Control.Concurrent
import Debug.Trace

import Control.Concurrent.STM qualified as STM

import Control.Concurrent.STM (STM, TVar)
import Control.Monad (void)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
    t <- STM.newTVar Nothing
    pure $ TMVar t

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
    t <- STM.newTVarIO Nothing
    pure $ TMVar t

takeTMVar :: (Show a) => TMVar a -> STM a
takeTMVar (TMVar t) = do
    mA <- STM.readTVar t
    traceShowM $ "Output: " <> show mA -- Called 4 times
    maybe STM.retry (\a -> STM.writeTVar t Nothing >> pure a) mA

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
    mA <- STM.readTVar t
    maybe (STM.writeTVar t (Just a)) (const STM.retry) mA

-- Experimenting with STM retries
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ta <- newEmptyTMVarIO
    tb <- newEmptyTMVarIO
    void $ forkIO $ do
        threadDelay (10 ^ 7)
        STM.atomically $ putTMVar tb 11

    res <- STM.atomically $ do
        putTMVar ta "a string"
        a <- takeTMVar ta
        -- traceShowM "Output" -- Called twice
        b <- takeTMVar tb
        pure (a, b)

    print @(String, Int) res
