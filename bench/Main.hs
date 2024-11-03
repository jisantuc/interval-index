{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad (join, replicateM)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Interval (Interval (..))
import qualified Data.IntervalIndex as IntervalIndex
import Data.List (zipWith4)
import GHC.Generics (Generic)
import System.Random (randomIO)

data BogusMetadata = BogusMetadata
  { bogusId :: String,
    size :: Int
  }
  deriving (Eq, Generic, Show)

instance NFData BogusMetadata

data BogusData = IntervalData
  { start :: Int,
    end :: Int,
    metadata :: BogusMetadata
  }
  deriving (Eq, Show, Generic)

instance NFData BogusData

instance Interval Int BogusData where
  intervalStart = start
  intervalEnd = end

-- TODO: rewrite around reading from test-data files in bench-compare
generateData :: Int -> IO [BogusData]
generateData numExamples =
  do
    starts <- replicateM numExamples randomIO
    increments <- replicateM numExamples ((`mod` 50) <$> randomIO)
    metadataSizes <- replicateM numExamples ((`mod` 50) <$> randomIO)
    ids <- replicateM numExamples (pure <$> randomIO)
    pure $
      zipWith4
        (\s i sz id' -> IntervalData s (s + i) (BogusMetadata id' sz))
        starts
        increments
        metadataSizes
        ids

main :: IO ()
main = defaultMain [benchN 10, benchN 100, benchN 1000, benchN 10000]

benchN :: Int -> Benchmark
benchN numIntervals =
  env
    (generateData numIntervals)
    ( \intervals ->
        bgroup
          (unwords [show numIntervals, "intervals"])
          [ bench "fromList" $ nf IntervalIndex.fromList intervals,
            mergeBench intervals,
            insertBench intervals
          ]
    )

mergeBench :: (Interval k a, NFData a, NFData k) => [a] -> Benchmark
mergeBench intervals =
  let makeIntervals = join bimap IntervalIndex.fromList
      (first10, last90) = makeIntervals $ splitAt (length intervals `div` 10) intervals
      (first30, last70) = makeIntervals $ splitAt (length intervals `div` 10) intervals
      (first50, last50) = makeIntervals $ splitAt (length intervals `div` 10) intervals
   in bgroup
        "merge"
        [ bench "10/90" $ nf (IntervalIndex.merge first10) last90,
          bench "30/70" $ nf (IntervalIndex.merge first30) last70,
          bench "50/50" $ nf (IntervalIndex.merge first50) last50,
          bench "70/30" $ nf (IntervalIndex.merge last70) first30,
          bench "90/10" $ nf (IntervalIndex.merge last90) first10
        ]

insertBench :: (Interval k a, NFData a, NFData k) => [a] -> Benchmark
insertBench intervals =
  let getSplit =
        ( \idx ->
            (IntervalIndex.fromList (take (idx - 1) intervals ++ drop (idx + 1) intervals), intervals !! idx)
        )
          . (`mod` length intervals)
          <$> randomIO
   in env getSplit $ \(~(idx, toInsert)) -> bench "insert" $ nf (IntervalIndex.insert idx) toInsert
