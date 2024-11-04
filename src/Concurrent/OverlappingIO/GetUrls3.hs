module Concurrent.OverlappingIO.GetUrls3 where

import Concurrent.OverlappingIO.Helpers.Async
import Concurrent.OverlappingIO.Helpers.GetUrl
import Concurrent.OverlappingIO.Helpers.TimeIt
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Text.Printf

sites :: [String]
sites =
    [ "http://www.google.com"
    , "http://www.bing.com"
    , "http://www.yahoo.com"
    , "http://www.wikipedia.com/wiki/Spade"
    , "http://www.wikipedia.com/wiki/Shovel"
    ]

timeDownload :: String -> IO ()
timeDownload url = do
    (page, time) <- timeIt . getUrl $ url
    printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main :: IO ()
main = do
    as <- traverse (async . timeDownload) sites
    traverse_ wait as
