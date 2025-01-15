module Concurrent.NetworkServers.STM where

import Control.Concurrent.STM (TVar, readTVar, atomically, readTVarIO, writeTVar, newTVarIO)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Network.Socket (Socket)
import Text.Read (readMaybe)
import Network.Run.TCP (runTCPServer)
import Data.String (IsString(..))
import Control.Monad (unless)

-- We can still rewrite this like Simon did but using a socket instead of a Handle
talk :: TVar Integer -> Socket -> IO ()
talk tFactor s = do
  _msg <- recv s 1024
  print _msg
  unless (BS.null _msg) $ do
    let msg = BSC.unpack $ parseCmd _msg
    putStrLn msg
    currentFactor <- readTVarIO tFactor
    handleReq tFactor currentFactor s msg
    talk tFactor s
 where
  parseCmd = BSC.takeWhile ((||) <$> (/= '\r') <*> (/= '\n'))

handleReq :: TVar Integer -> Integer -> Socket -> String -> IO ()
handleReq tFactor f s msg = do
  currentFactor <- readTVarIO tFactor
  sendAll s $ "Current factor: " <> (fromString . show $ currentFactor) <> "\n"
  serviceReq currentFactor msg
 where
  serviceReq currentFactor cmd = do
    runCmdAction <- atomically $ do
      latestFactor <- readTVar tFactor
      if currentFactor /= latestFactor
        then pure $ notifyOfNewFactor latestFactor
        else pure $ handleClientInput latestFactor

    runCmdAction cmd

  notifyOfNewFactor newFactor _ = do
    sendToClient $ "new factor: " <> (fromString . show $ newFactor) <> "\n"

  sendToClient = sendAll s

  handleClientInput actualFactor cmd = case cmd of
    "end" -> sendToClient "Thank you for using the Haskell doubling service!"
    multCmd@('*':n) -> do
      let mNum = readMaybe @Integer n
      case mNum of
        Nothing -> sendToClient $ fromString multCmd <> " is not a valid multiplication command"
        Just num -> atomically $ writeTVar tFactor num
    line -> do
      let mNum = readMaybe @Integer line
      case mNum of
        Nothing -> sendToClient $ fromString line <> " is not a valid number"
        Just num -> do
          sendToClient $ fromString $ "Input: " <> line
          sendToClient $ fromString $ "Output: " <> show (actualFactor * num) <> "\n\n"



main :: IO ()
main = do
  tFactor <- newTVarIO (2 :: Integer)
  runTCPServer Nothing "4444" $ \s -> do
    sendAll s "Welcome to the Haskell doubling service!\n"
    talk tFactor s
