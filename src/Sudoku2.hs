module Sudoku2 where

import Control.DeepSeq (force)
import Control.Parallel.Strategies
import Data.Maybe
import Sudoku
import System.Environment

main :: IO ()
main = do
    [f] <- getArgs
    file <- readFile f

    let puzzles = lines file
    let (as, bs) = splitAt (length puzzles `div` 2) puzzles
    let solutions = runEval $ do
            as' <- rpar (force . mapMaybe solve $ as)
            bs' <- rpar (force . mapMaybe solve $ bs)
            rseq (as' ++ bs')

    print . length $ solutions
