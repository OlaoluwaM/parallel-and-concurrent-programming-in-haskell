module Concurrent.OverlappingIO.Helpers.Async2 where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import Data.Foldable (traverse_)

newtype Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    resultVar <- newEmptyMVar
    void $ forkIO $ try action >>= putMVar resultVar
    pure $ Async resultVar

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async pendingIOCompVar) = readMVar pendingIOCompVar

wait :: Async a -> IO a
wait a = do
    result <- waitCatch a
    either throwIO pure result

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither aA aB = do
    m <- newEmptyMVar
    void $ forkIO $ try (fmap Left (wait aA)) >>= putMVar m
    void $ forkIO $ try (fmap Right (wait aB)) >>= putMVar m
    wait (Async m)

-- We can use STM to come up with a more efficient implementation of `waitAny` to avoid having to create a new thread for each async action
waitAny :: [Async a] -> IO a
waitAny asyncs = do
    m <- newEmptyMVar
    traverse_ (forkWait m) asyncs
    wait (Async m)
  where
    forkWait m a = try (wait a) >>= putMVar m
