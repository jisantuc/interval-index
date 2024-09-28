{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Criterion.Main (bench, bgroup, defaultMain, nf)
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

generateData :: IO [BogusData]
generateData =
  let numExamples = 100000
   in do
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
main =
  do
    intervals <- generateData
    let first10 = take 10 intervals
    let first100 = take 100 intervals
    let first1k = take 1000 intervals
    let first10k = take 10000 intervals
    defaultMain
      [ bgroup
          "fromList"
          [ bench "10" $ nf IntervalIndex.fromList first10,
            bench "100" $ nf IntervalIndex.fromList first100,
            bench "1000" $ nf IntervalIndex.fromList first1k,
            bench "10000" $ nf IntervalIndex.fromList first10k
          ]
      ]
