module Concurrent.OverlappingIO.GetUrlsCancel where

import Concurrent.OverlappingIO.GetUrls3 (timeDownload)
import Concurrent.OverlappingIO.Helpers.Async3
import Concurrent.OverlappingIO.Helpers.GetUrl
import Control.Concurrent
import Control.Monad
import Data.ByteString qualified as B
import Data.Either (rights)
import Data.Foldable (traverse_)
import System.IO
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
    as <- traverse (async . timeDownload) sites

    void $ forkIO $ do
        hSetBuffering stdin NoBuffering
        forever $ do
            char <- getChar
            when (char == 'q') $ putStrLn "\r" >> mapM_ cancel as

    rs <- traverse waitCatch as
    printf "%d/%d succeeded\n" (length (rights rs)) (length rs)
