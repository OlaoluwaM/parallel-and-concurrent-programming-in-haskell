module Concurrent.OverlappingIO.Helpers.Async3 where

import Control.Concurrent
import Control.Exception
import Control.Monad (join, void)
import Data.Either.Extra (fromEither)
import Data.Foldable (traverse_)

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    resultVar <- newEmptyMVar
    threadId <- forkIO $ try action >>= putMVar resultVar
    pure $ Async threadId resultVar

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ pendingIOCompVar) = readMVar pendingIOCompVar

wait :: Async a -> IO (ThreadId, a)
wait a@(Async threadId _) = do
    result <- waitCatch a
    either throwIO (pure . (threadId,)) result

-- waitEither :: Async a -> Async b -> IO (Either a b)
-- waitEither aA aB = do
--     m <- newEmptyMVar
--     void $ forkIO $ try (fmap Left (wait aA)) >>= putMVar m
--     void $ forkIO $ try (fmap Right (wait aB)) >>= putMVar m
--     threadId <- either fst fst . join <$> readMVar m
--     wait (Async threadId (fmap (fmap snd . join) m))

-- We can use STM to come up with a more efficient implementation of `waitAny` to avoid having to create a new thread for each async action
-- waitAny :: [Async a] -> IO a
-- waitAny asyncs = do
--     m <- newEmptyMVar
--     traverse_ (forkWait m) asyncs
--     wait (Async m)
--   where
--     forkWait m a = try (wait a) >>= putMVar m

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled
