module Concurrent.OverlappingIO.MaskExample where

import Concurrent.OverlappingIO.GetUrls3 (timeDownload)
import Concurrent.OverlappingIO.Helpers.Async3
import Concurrent.OverlappingIO.Helpers.GetUrl
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.Either (rights)
import Data.Foldable (traverse_)
import System.Exit (ExitCode (ExitFailure))
import System.IO
import Text.Printf (printf)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    m <- newEmptyMVar
    threadId <- mask $ \restore -> forkIO $ do
        putStrLn "Executing new thread..."
        threadDelay 1
        putMVar m 900
        g <- takeMVar m
        putStrLn "Heading to restored region"
        restore (putStrLn "Doing that again") `catch` (\e -> putStrLn "Exception!" >> print @SomeException e >> throw e)
        threadDelay 1
        putMVar m (g * 900000)
        putStrLn "Done in concurrent thread"

    void $ forkIO $ do
        threadDelay 22
        throwTo threadId (ErrorCall "Custom")
        putStrLn "Done in thread 2"

    f <- getChar
    putStrLn $ "Done in main thread " <> [f]

-- main :: IO ()
-- main = do
--     hSetBuffering stdout NoBuffering
--     m <- newEmptyMVar
--     threadId <- forkIO $ do
--         putStrLn "Executing new thread..."
--         threadDelay (10 ^ 2)
--         putMVar m 900
--         g <- takeMVar m
--         putStrLn "Doing that again" `catch` (\e -> putStrLn "Exception!" >> print @SomeException e >> throw e)
--         threadDelay (10 ^ 7)
--         putMVar m (g * 900000)
--         putStrLn "Done in concurrent thread"

--     -- threadDelay (10 ^ 1)
--     throwTo threadId (ErrorCall "Custom")
--     threadDelay (10 ^ 4)
--     putStrLn "Done"
