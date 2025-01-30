module Parallel.FileSearch.FindFileParIO where

import Data.List qualified as L

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Environment (getArgs)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import Control.Monad.Par.Class (ParIVar(new, put, fork), get)
import Control.Monad.IO.Class (MonadIO(liftIO))


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
subFind :: String -> FilePath -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)) -> [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
subFind filename initialSearchDirPath inner ivars = do
  isDir <- liftIO $ doesDirectoryExist initialSearchDirPath
  if not isDir
    then inner ivars
    else do
      ivar <- new
      fork (find filename initialSearchDirPath >>= put ivar)
      inner (ivar : ivars)


find :: String -> FilePath -> ParIO (Maybe FilePath)
find filename initialSearchDirPath = do
    dirContents <- L.sort . filter (`notElem` [".", ".."]) <$> liftIO (getDirectoryContents initialSearchDirPath)
    let fileIsPartOfCurrDirContents = filename `elem` dirContents
    if fileIsPartOfCurrDirContents
        then pure (Just $ initialSearchDirPath </> filename)
        else do
          let paths = map (initialSearchDirPath </>) dirContents
          foldr (subFind filename) doWait paths []
  where
    doWait = loop . reverse

    loop [] = pure Nothing
    loop (iv:ivs) = do
        result <- get iv
        maybe (loop ivs) (pure . Just) result

main :: IO ()
main = do
  [s,d] <- getArgs
  runParIO (find s d) >>= print
