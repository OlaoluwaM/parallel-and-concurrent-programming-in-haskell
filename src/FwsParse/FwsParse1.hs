{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module FwsParse.FwsParse1 (main, runtest) where

import Control.Monad.Par.Scheds.Trace

-- gives slightly better results than Control.Monad.Par with monad-par-0.3.4

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as Map
import Data.List qualified as L
import System.Environment
import System.Random

import Data.Foldable
import FwsParse.SparseGraph

-- -----------------------------------------------------------------------------
-- shortestPaths

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = runPar $ foldlM update g vs
  where
    -- <<update
    update g k = do
        m <- Map.traverseWithKey (\i jmap -> spawn (pure (shortmap i jmap))) g
        traverse get m
      where
        -- >>

        shortmap :: Vertex -> IntMap Weight -> IntMap Weight
        shortmap i jmap = L.foldr shortest Map.empty vs
          where
            shortest j m =
                case (old, new) of
                    (Nothing, Nothing) -> m
                    (Nothing, Just w) -> Map.insert j w m
                    (Just w, Nothing) -> Map.insert j w m
                    (Just w1, Just w2) -> Map.insert j (min w1 w2) m
              where
                old = Map.lookup j jmap
                new = do
                    w1 <- weight g i k
                    w2 <- weight g k j
                    return (w1 + w2)

-- -----------------------------------------------------------------------------
-- Testing

test :: [[Int]]
test =
    [ [0, 999, 999, 13, 999, 999]
    , [999, 0, 999, 999, 4, 9]
    , [11, 999, 0, 999, 999, 999]
    , [999, 3, 999, 0, 999, 7]
    , [15, 5, 999, 1, 0, 999]
    , [11, 999, 999, 14, 999, 0]
    ]

-- correct result:
-- [ [0,  16, 999, 13, 20, 20],
--   [19,  0, 999,  5,  4,  9],
--   [11, 27,   0, 24, 31, 31],
--   [18,  3, 999,  0,  7,  7],
--   [15,  4, 999,  1,  0,  8],
--   [11, 17, 999, 14, 21,  0] ]

runtest :: [[Int]]
runtest = fromAdjMatrix (toAdjMatrix 5 (shortestPaths [0 .. 5] (mkGraph test)))

main :: IO ()
main = do
    [h, n] <- fmap (fmap read) getArgs
    let g = mkStdGen 9999
    let (mat, vs) = randomGraph g h 100 n
    print (checksum (shortestPaths vs mat))
