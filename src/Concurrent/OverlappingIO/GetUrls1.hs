module Concurrent.OverlappingIO.GetUrls1 where

import Concurrent.OverlappingIO.Helpers.GetUrl
import Control.Concurrent
import Control.Monad
import Data.ByteString qualified as B

main :: IO ()
main = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar

    void $ forkIO $ getUrl "http://www.wikipedia.org/wiki/Shovel" >>= putMVar m1
    void $ forkIO $ getUrl "http://www.wikipedia.org/wiki/Spade" >>= putMVar m2

    r1 <- takeMVar m1
    r2 <- takeMVar m2

    print (B.length r1, B.length r2)
