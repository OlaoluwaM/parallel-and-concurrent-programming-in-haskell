module Concurrent.ThreadsAndMVars.Fork where

import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    void $ forkIO (replicateM_ 100 (putChar 'A'))
    replicateM_ 100 (putChar 'B')
