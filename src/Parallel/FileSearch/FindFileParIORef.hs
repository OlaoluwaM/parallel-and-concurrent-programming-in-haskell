module Parallel.FileSearch.FindFileParIORef where

import Data.List qualified as L

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Async, withAsync, wait)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Exception (finally)
import System.FilePath ((</>))
import System.Environment (getArgs)
import Data.IORef (IORef, newIORef, atomicModifyIORef)


newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem resourceCount = do
  m <- newIORef resourceCount
  pure $ NBSem m

-- To acquire a unit of a resource from the semaphore
tryAcquireNBSem  :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) = do
  atomicModifyIORef m $ \count -> if count == 0
    then (count, False)
    else let !z = count - 1 in (z, True)

-- To release a previously acquired unit of a resource from the semaphore
releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) = do
  atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())

-- Using the semaphore in `subFind` as this is where the task/decision of whether to make new thread
subFind :: NBSem -> String -> FilePath -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath)) -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subFind nbSem filename initialSearchDirPath inner asyncs = do
  isDir <- doesDirectoryExist initialSearchDirPath
  if not isDir
    then inner asyncs
    else do
      unitOfResource <- tryAcquireNBSem nbSem
      if unitOfResource
        then do
          let doFind = find nbSem filename initialSearchDirPath `finally` releaseNBSem nbSem
          withAsync doFind $ \a -> inner (a:asyncs)
        else do
          result <- find nbSem filename initialSearchDirPath
          maybe (inner asyncs) (pure . Just) result

find :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
find nbSem filename initialSearchDirPath = do
    dirContents <- L.sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents initialSearchDirPath
    let fileIsPartOfCurrDirContents = filename `elem` dirContents
    if fileIsPartOfCurrDirContents
        then pure (Just $ initialSearchDirPath </> filename)
        else do
          let paths = map (initialSearchDirPath </>) dirContents
          foldr (subFind nbSem filename) doWait paths []
  where
    doWait as = loop (reverse as)

    loop [] = pure Nothing
    loop (a:as) = do
        result <- wait a
        maybe (loop as) (pure . Just) result

main :: IO ()
main = do
  [s,d] <- getArgs
  n <- getNumCapabilities
  sem <- newNBSem (if n == 1 then 0 else n * 4)
  find sem s d >>= print
