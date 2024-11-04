module Concurrent.OverlappingIO.GetUrls5 where

import Concurrent.OverlappingIO.Helpers.GetUrl
import Control.Concurrent
import Control.Monad (replicateM_)
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Text.Printf (printf)

-- The idea here is that we want to fire multiple actions but immediately start performing some operation on the first action to be completed, then on the second, then the third and so on. Essentially we do not want to wait for the *all* the actions to resolve before operating on their result, rather we want to begin operating on the result as soon as one of those actions returns something. Kinda like streaming...

-- This is somewhat similar to the semantics of `Promise.race` in JavaScript

sites :: [String]
sites =
    [ "http://www.google.com"
    , "http://www.bing.com"
    , "http://www.yahoo.com"
    , "http://www.wikipedia.com/wiki/Spade"
    , "http://www.wikipedia.com/wiki/Shovel"
    ]

main :: IO ()
main = do
    m <- newEmptyMVar
    traverse_ (forkIO . download m) sites

    (url, result) <- takeMVar m
    printf "%s was first (%d bytes)\n" url (B.length result)
    replicateM_ (length sites - 1) (takeMVar m)
  where
    download var url = getUrl url >>= putMVar var . (url,)
