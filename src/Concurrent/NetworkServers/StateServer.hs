{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Concurrent.NetworkServers.StateServer where

import Control.Applicative (Alternative ((<|>)), asum)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Monad (forever, join, void)
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.Attoparsec.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Functor (($>))
import Data.String (IsString (fromString))
import Data.String.Interpolate (i)
import Network.Run.TCP (runTCPServer)
import Network.Socket (SockAddr, Socket, getPeerName)
import Network.Socket.ByteString (recv, sendAll)

newtype FactorState = FactorState {currentFactor :: TVar Int}

data ClientMessage = FactorUpdate Int | MultReq Int | CloseConnection | SomeText String

main :: IO ()
main = do
    currentFactor <- newTVarIO 3
    let factorState = FactorState{currentFactor}
    runTCPServer Nothing "4444" $ handleConnection factorState

handleConnection :: FactorState -> Socket -> IO ()
handleConnection factorState s = do
    commsChan <- newTChanIO
    -- The race here treats these two actions as sibling threads so if one exits, the other exits as well
    void $ race (handleReq s commsChan factorState) (receiveClientMsgs commsChan s)

receiveClientMsgs :: TChan ClientMessage -> Socket -> IO ()
receiveClientMsgs commsChan s = forever $ do
    msg <- recv s 1024
    let clientMsg = AC.parseOnly (clientMsgParser msg) msg
    atomically $ writeTChan commsChan (either SomeText id clientMsg)

clientMsgParser :: ByteString -> AC.Parser ClientMessage
clientMsgParser v =
    let str = BC.unpack v
     in asum
            [ withCustomError
                ((AC.string "end" <* AC.endOfLine) $> CloseConnection)
                [i|#{str} is not a valid connection termination command|]
            , withCustomError
                (FactorUpdate <$> (AC.char '*' *> AC.decimal @Int <* AC.endOfLine))
                [i|#{str} is not a valid factor update command|]
            , withCustomError (MultReq <$> (AC.decimal @Int <* AC.endOfLine)) [i|#{str} is not a valid number|]
            ]
  where
    withCustomError :: Parser i a -> String -> Parser i a
    withCustomError p s = p <|> fail s

handleReq :: Socket -> TChan ClientMessage -> FactorState -> IO ()
handleReq s commsChan factorState = do
    currentFactor <- readTVarIO factorState.currentFactor
    client <- getPeerName s
    putStrLn [i|#{client} just connected|]
    sendAll s [i|Current multiplication factor: #{currentFactor}\n|]
    go currentFactor client
  where
    go :: Int -> SockAddr -> IO ()
    go factorValue client = do
        join $ atomically $ do
            factorValueAsOfNow <- readTVar factorState.currentFactor
            if factorValue /= factorValueAsOfNow
                then pure (notifyClientsOfNewFactor factorValueAsOfNow client)
                else do
                    clientMsg <- readTChan commsChan
                    pure (handleClientMsg factorValueAsOfNow client clientMsg)

    notifyClientsOfNewFactor :: Int -> SockAddr -> IO ()
    notifyClientsOfNewFactor newFactorValue client = do
        sendAll s [i|New factor: #{newFactorValue}\n|]
        go newFactorValue client

    handleClientMsg :: Int -> SockAddr -> ClientMessage -> IO ()
    handleClientMsg currentFactor client = \case
        CloseConnection -> do
            sendAll s "Thank you for using the Haskell doubling service\n"
            putStrLn [i|#{client} just disconnected|]
        FactorUpdate newFactor -> do
            atomically $ writeTVar factorState.currentFactor newFactor
            let oldFactor = currentFactor
            go oldFactor client -- We call `go` with the old factor to force the notifyClientsOfNewFactor to be called
        MultReq numToMultiply -> do
            putStrLn [i|#{currentFactor * numToMultiply}|]
            go currentFactor client
        SomeText txt -> do
            sendAll s (fromString txt <> "\n")
            go currentFactor client
