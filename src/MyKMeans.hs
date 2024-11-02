{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MyKMeans where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List qualified as L
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MVector
import Text.Printf (printf)

data Point = Point !Double !Double deriving (Eq, Show)

zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

-- Where `clCent` represents a cluster's centroid
data Cluster = Cluster {clId :: Int, clCent :: Point} deriving (Eq, Show)

data PointSum = PointSum !Int !Double !Double deriving (Eq, Show)

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y) = PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) = Cluster i (Point (xs / fromIntegral count) (ys / fromIntegral count))

assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nClusters clusters points = V.create $ do
    newClusters <- MVector.replicate nClusters (PointSum 0 0 0)
    let
        addPointToCluster targetPoint = do
            let clusterNearestTargetPoint = getNearestClusterToPoint targetPoint
            pointsAlreadyInNearestCluster <- MVector.read newClusters clusterNearestTargetPoint.clId
            MVector.write newClusters clusterNearestTargetPoint.clId $! addToPointSum pointsAlreadyInNearestCluster targetPoint

    mapM_ addPointToCluster points
    pure newClusters
  where
    getNearestClusterToPoint targetPoint = fst . L.minimumBy (compare `on` snd) . map (id &&& (\cluster -> sqDistance cluster.clCent targetPoint)) $ clusters

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec = [pointSumToCluster i ps | (i, ps@(PointSum count _ _)) <- V.toList (V.indexed vec), count > 0]

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points = makeNewClusters (assign nclusters clusters points)

kmeans_seq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
  let
    loop :: Int -> [Cluster] -> IO [Cluster]
    loop n clusters
      | n > tooMany = do
        putStrLn "giving up."
        pure clusters
     |  otherwise = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = step nclusters clusters points
        if clusters' == clusters
          then return clusters
          else loop (n+1) clusters'
  in loop 0 clusters
 where
  tooMany = 80
