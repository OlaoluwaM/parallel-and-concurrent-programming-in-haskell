module Parallel.Sudoku1 where

import Data.Maybe
import Parallel.Sudoku
import System.Environment

main :: IO ()
main = do
    [f] <- getArgs
    file <- readFile f

    let puzzles = lines file
    let solutions = mapMaybe solve puzzles

    print . length $ solutions
