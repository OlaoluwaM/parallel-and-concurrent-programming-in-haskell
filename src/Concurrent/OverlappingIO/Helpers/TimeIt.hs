-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Returns the number of realtime seconds an action takes

module Concurrent.OverlappingIO.Helpers.TimeIt (timeIt) where

import Data.Time

timeIt :: IO a -> IO (a, Double)
timeIt io = do
    t0 <- getCurrentTime
    a <- io
    t1 <- getCurrentTime
    return (a, realToFrac (t1 `diffUTCTime` t0))
