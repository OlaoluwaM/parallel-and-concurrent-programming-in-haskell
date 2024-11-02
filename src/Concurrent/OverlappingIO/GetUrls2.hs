module Concurrent.OverlappingIO.GetUrls2 where

import Control.Concurrent
import Control.Monad (void)

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async ioAction = do
    var <- newEmptyMVar
    void $ forkIO $ ioAction >>= putMVar var
    pure (Async var)

wait :: Async a -> IO a
wait (Async m) = readMVar m
