module Concurrent.STM.TooManyBlocking where

import Control.Concurrent.STM.TMVar
import Control.Monad (void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Concurrent.STM (atomically)
import Data.Vector qualified as V

-- Fast because we have multiple transactions that can unblock or block independently of each other (I think)
main :: IO ()
main = do
  tmvars <- V.replicateM 10000000 newEmptyTMVarIO

  void $ forkIO $ do
    threadDelay (10 ^ 3)
    putDelay <- newDelay (10 ^ 5)
    V.iforM_ tmvars $ \ind tmvar -> atomically $ do
      waitDelay putDelay
      let place = ind + 1
      putTMVar tmvar ("text" <> show place)

  putStrLn "Now running on main thread"
  mapM_ (atomically . takeTMVar) tmvars
  putStrLn "Done"

-- Slow because we have a single transaction comprised of too many blocking operations
-- main :: IO ()
-- main = do
--   tmvars <- V.replicateM 10000000 newEmptyTMVarIO

--   void $ forkIO $ do
--     threadDelay (10 ^ 3)
--     putDelay <- newDelay (10 ^ 5)
--     V.iforM_ tmvars $ \ind tmvar -> atomically $ do
--       waitDelay putDelay
--       let place = ind + 1
--       putTMVar tmvar ("text" <> show place)

--   putStrLn "Now running on main thread"
--   void $ atomically $ mapM takeTMVar tmvars
--   putStrLn "Done"
