{-# LANGUAGE OverloadedStrings #-}

module Concurrent.NetworkServers.Trivial where

import Control.Arrow ((&&&))
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.Foldable (for_)
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket, close')
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

-- Regular server
regServer :: IO ()
regServer = do
    runTCPServer Nothing "4444" talk
  where
    talk :: Socket -> IO ()
    talk s = do
        msg <- recv s 1024
        if msg == "end\n" || B.null msg
            then putStrLn "Thank you for using the Haskell doubling service.\n"
            else do
                let ns = map ((id &&& readMaybe @Int) . strip . BC.unpack) $ BC.lines msg
                for_ ns $ \(str, n) -> maybe (putStrLn (str <> " is not a valid number. Please try again")) (print . (* 2)) n
                talk s

-- Limiting connections with a semaphore
limitServer :: IO ()
limitServer = do
    connectionCount <- newMVar @Int 0
    runTCPServer Nothing "4444" (talk connectionCount)
  where
    talk :: MVar Int -> Socket -> IO ()
    talk cCountMVar s = do
        cCount <- readMVar cCountMVar
        if cCount >= connectionLimit
            then do
                sendAll s "Servicing max number of clients...Please wait\n"
                close' s
            else do
                modifyMVar_ cCountMVar (pure . (+ 1))
                loop s
                modifyMVar_ cCountMVar (pure . subtract 1)

    loop :: Socket -> IO ()
    loop s = do
        msg <- recv s 1024
        if msg == "end\n" || B.null msg
            then putStrLn "Thank you for using the Haskell doubling service.\n"
            else do
                let ns = map ((id &&& readMaybe @Int) . strip . BC.unpack) $ BC.lines msg
                for_ ns $ \(str, n) -> maybe (putStrLn (str <> " is not a valid number. Please try again")) (print . (* 2)) n
                loop s

    connectionLimit :: Int
    connectionLimit = 2

main :: IO ()
main = limitServer

strip :: String -> String
strip = rStrip . lStrip
  where
    lStrip :: String -> String
    lStrip = dropWhile isSpace

    rStrip :: String -> String
    rStrip = reverse . dropWhile isSpace . reverse
