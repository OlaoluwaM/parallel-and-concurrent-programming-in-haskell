module Sudoku3 where

import Control.Exception (evaluate)
import Control.Parallel.Strategies
import Data.Maybe (catMaybes)
import Sudoku
import System.Environment
import Control.DeepSeq (force)

main :: IO ()
main = do
    [f] <- getArgs
    file <- readFile f >>= evaluate

    let puzzles = force . lines $ file
    let solutions = runEval (traverse (rpar . solve) puzzles)

    print . length . catMaybes $ solutions
