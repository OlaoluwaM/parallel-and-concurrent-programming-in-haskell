module Concurrent.STM.GetUrlsSTM where

import Concurrent.OverlappingIO.Helpers.GetUrl
import Concurrent.STM.AsyncSTM
import Control.Concurrent.STM (atomically)
import Data.ByteString qualified as B
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
    as <- mapM (async . download) sites
    (url, r) <- waitAny as

    printf "%s was first (%d bytes)\n" url (B.length r)
    mapM_ (\a -> atomically (waitSTM a) >>= print . fst) as
  where
    download url = (url,) <$> getUrl url
