module Concurrent.OverlappingIO.GetUrls2 where

import Concurrent.OverlappingIO.Helpers.GetUrl
import Concurrent.OverlappingIO.Helpers.Async
import Data.ByteString qualified as B

main :: IO ()
main = do
  ac1 <- async $ getUrl "http://www.wikipedia.org/wiki/Shovel"
  ac2 <- async $ getUrl "http://www.wikipedia.org/wiki/Spade"

  r1 <- wait ac1
  r2 <- wait ac2

  print (B.length r1, B.length r2)
