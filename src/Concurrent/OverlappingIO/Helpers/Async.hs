module Concurrent.OverlappingIO.Helpers.Async (
    async,
    wait,
) where

import Control.Concurrent
import Control.Monad (void)

-- Represents an async action that has been started/initiated
newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async ioAction = do
    var <- newEmptyMVar
    void $ forkIO $ ioAction >>= putMVar var
    pure (Async var)

-- We want to use `readMVar` instead of `takeMVar` here to allow for the possibility of multiple clients wanting to reference/access the result of the same async action. If we were to use `takeMVar` instead, we'd end up in a situation where only the first `wait` call will receive the async computation value. The other calls to `wait` on the same async comp would block indefinitely causing a deadlock.
-- Once again, when an async computation is no longer referenced it'll be garbage collected
wait :: Async a -> IO a
wait (Async m) = readMVar m
