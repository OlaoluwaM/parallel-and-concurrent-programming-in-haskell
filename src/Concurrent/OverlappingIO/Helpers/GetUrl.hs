-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- Simple wrapper around HTTP, allowing proxy use

module Concurrent.OverlappingIO.Helpers.GetUrl (getUrl) where

import Control.Applicative -- for GHC < 7.10
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Network.HTTP.Conduit

getUrl :: String -> IO ByteString
getUrl url = L.toStrict <$> simpleHttp url
