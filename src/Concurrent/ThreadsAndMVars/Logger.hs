module Concurrent.ThreadsAndMVars.Logger where

import Control.Concurrent
import Control.Monad (void)

newtype Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let l = Logger m
    void $ forkIO (logger l)
    pure l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
        cmd <- takeMVar m
        case cmd of
            Message msg -> putStrLn msg >> loop
            Stop s -> putStrLn "Stopping logger..." >> putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s

main :: IO ()
main = do
    l <- initLogger
    logMessage l "hello"
    logMessage l "bye"
    logStop l
