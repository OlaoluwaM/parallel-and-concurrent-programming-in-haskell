module Parallel.FileSearch.FindFilePar where

import Data.List qualified as L
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Control.Concurrent.Async (Async, withAsync, wait)

find :: String -> FilePath -> IO (Maybe FilePath)
find filename initialSearchDirPath = do
    dirContents <- L.sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents initialSearchDirPath
    let fileIsPartOfCurrDirContents = filename `elem` dirContents
    if fileIsPartOfCurrDirContents
        then pure (Just $ initialSearchDirPath </> filename)
        else do
          let paths = map (initialSearchDirPath </>) dirContents
          foldr (subFind filename) doWait paths []
  where
    doWait as = loop (reverse as)

    loop [] = pure Nothing
    loop (a:as) = do
        result <- wait a
        maybe (loop as) (pure . Just) result

subFind :: String -> FilePath -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath)) -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subFind filename initialSearchDirPath inner asyncs = do
    isDir <- doesDirectoryExist initialSearchDirPath
    if not isDir
        then inner asyncs
        else withAsync (find filename initialSearchDirPath) $ \a -> inner (a:asyncs)

main :: IO ()
main = do
    [filename, initialSearchDirPath] <- getArgs
    result <- find filename initialSearchDirPath
    print result
