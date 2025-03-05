module Concurrent.OverlappingIO.GetUrlsChan where

import Concurrent.OverlappingIO.Helpers.GetUrl
import Control.Concurrent
import Control.Monad (replicateM_)
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Text.Printf (printf)

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
    resultChan <- newChan
    traverse_ (forkIO . download resultChan) sites

    (url, result) <- readChan resultChan
    printf "%s was first (%d bytes)\n" url (B.length result)
    replicateM_ (length sites - 1) $ do
        (otherUrl, r) <- readChan resultChan
        printf "followed by %s (%d bytes)\n" otherUrl (B.length r)
  where
    download chan url = getUrl url >>= writeChan chan . (url,)
