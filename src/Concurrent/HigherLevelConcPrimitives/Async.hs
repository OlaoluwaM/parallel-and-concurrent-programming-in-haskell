module Concurrent.HigherLevelConcPrimitives.Async where

import Control.Exception (bracket, SomeException, AsyncException (ThreadKilled))
import Control.Concurrent.STM (STM, atomically, orElse, retry, TMVar, newEmptyTMVarIO, putTMVar, readTMVar, throwSTM)
import Control.Concurrent (threadDelay, ThreadId, forkFinally, throwTo)

data Async a = Async ThreadId (STM (Either SomeException a))

instance Functor Async where
  fmap f (Async t inner) = Async t ((fmap . fmap) f inner)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    t <- forkFinally action (atomically . putTMVar var)
    pure $ Async t (readTMVar var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ stm) = stm

waitSTM :: Async a -> STM a
waitSTM a = do
    r <- waitCatchSTM a
    either throwSTM pure r

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io = bracket (async io) cancel

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a1 a2 = atomically $ do
  r1 <- waitSTM a1 `orElse` (waitSTM a2 >> retry)
  r2 <- waitSTM a2
  pure (r1, r2)

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $ (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

concurrently :: IO a -> IO b -> IO (a, b)
concurrently ioa iob = withAsync ioa $ \a -> withAsync iob $ \b -> waitBoth a b

race :: IO a -> IO b -> IO (Either a b)
race ioa iob = withAsync ioa $ \a -> withAsync iob $ \b -> waitEither a b

timeout :: Int -> IO a -> IO (Maybe a)
timeout n action
  | n <= 0 = fmap Just action
  | otherwise = do
    eitherR <- race (threadDelay n) action
    pure . either (const Nothing) Just $ eitherR

waitAny :: [Async a] -> IO a
waitAny = atomically . foldr (orElse . waitSTM) retry
