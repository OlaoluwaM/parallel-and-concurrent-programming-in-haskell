module Parallel.FileSearch.FindFileSeq where

import Data.List qualified as L
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

findSeq :: String -> FilePath -> IO (Maybe FilePath)
findSeq filename initialSearchDirPath = do
    dirContents <- L.sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents initialSearchDirPath
    let fileIsPartOfCurrDirContents = filename `elem` dirContents
    if fileIsPartOfCurrDirContents
        then pure (Just $ initialSearchDirPath </> filename)
        else loop dirContents
  where
    loop [] = pure Nothing
    loop (potentialDirName : otherPotentialDirNames) = do
        let fullPotentialDirPath = initialSearchDirPath </> potentialDirName
        isDir <- doesDirectoryExist fullPotentialDirPath
        if isDir
            then do
                result <- findSeq filename fullPotentialDirPath
                maybe (loop otherPotentialDirNames) (pure . pure) result
            else loop otherPotentialDirNames

main :: IO ()
main = do
    [filename, initialSearchDirPath] <- getArgs
    result <- findSeq filename initialSearchDirPath
    print result
