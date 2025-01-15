module Concurrent.NetworkServers.Trivial where

import System.IO (hSetBuffering, BufferMode (LineBuffering), Handle, hGetLine, hPutStrLn, IOMode (ReadWriteMode), hClose)
import Text.Read (readMaybe)
import Network.Run.TCP (runTCPServer)
import Network.Socket (socketToHandle)

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
 where
  loop = do
    line <- hGetLine h
    print line
    if line == "end"
      then hPutStrLn h "Thank you for using the Haskell doubling service!" >> hClose h
      else handleInput line >> loop

  handleInput inp = do
    let inputVal = readMaybe @Integer inp
    let outputVal = maybe (inp <> " is an invalid number") (show . (*2)) inputVal
    hPutStrLn h outputVal

main :: IO ()
main = runTCPServer Nothing "4444" handleReq
 where
  handleReq s = do
    -- Note that using handles for network programming in haskell isn't recommended since handle operations might not respect protocol operations and semantics like shutdown procedures and so on
    h <- socketToHandle s ReadWriteMode
    talk h
