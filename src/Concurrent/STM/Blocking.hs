module Concurrent.STM.Blocking where

import Control.Concurrent
import Debug.Trace

import Control.Concurrent.STM qualified as STM

import Control.Concurrent.STM (STM, TVar)
import Control.Monad (void)
import GHC.IO (unsafePerformIO)
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

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
    mA <- STM.readTVar t
    maybe STM.retry (\a -> STM.writeTVar t Nothing >> pure a) mA

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
    mA <- STM.readTVar t
    maybe (STM.writeTVar t (Just a)) (const STM.retry) mA

readTMVar :: TMVar a -> STM a
readTMVar (TMVar t) = do
    mA <- STM.readTVar t
    maybe STM.retry pure mA
