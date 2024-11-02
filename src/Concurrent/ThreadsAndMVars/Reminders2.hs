module Concurrent.ThreadsAndMVars.Reminders2 where

import Control.Concurrent
import Control.Monad
import Text.Printf

main :: IO ()
main = loop
  where
    loop = do
        s <- getLine
        case s of
            "exit" -> pure ()
            _ -> void (forkIO $ setReminder s) >> loop

setReminder :: String -> IO ()
setReminder s = do
    let t = read s :: Int
    printf "Okay, I'll remind you in %d seconds\n" t
    threadDelay (10 ^ 6 * t)
    printf "%d seconds is up! BING!\BEL\n" t
