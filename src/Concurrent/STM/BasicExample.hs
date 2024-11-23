module Concurrent.STM.BasicExample where

import Control.Concurrent.STM

main :: IO ()
main = do
    var <- newTVarIO "initial value"

    n <- atomically $ do
        -- There is no concept of "take" in STM like there is with MVars
        -- So locks don't really exist, at least not to the user, but they can be emulated using the retry operation
        v <- readTVar var
        k <- readTVar var
        pure (v, k)

    print n
