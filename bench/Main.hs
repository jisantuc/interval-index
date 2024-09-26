{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq (NFData)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Aeson (FromJSON, throwDecode')
import qualified Data.ByteString.Lazy as BS
import Data.Interval (Interval (..))
import qualified Data.IntervalIndex as IntervalIndex
import GHC.Generics (Generic)

data BogusMetadata = BogusMetadata
  { bogusId :: String,
    size :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON BogusMetadata

instance NFData BogusMetadata

data BogusData = IntervalData
  { start :: Int,
    end :: Int,
    metadata :: BogusMetadata
  }
  deriving (Eq, Show, Generic)

instance FromJSON BogusData

instance NFData BogusData

instance Interval Int BogusData where
  intervalStart = start
  intervalEnd = end

-- example blog post:
-- http://www.serpentine.com/criterion/tutorial.html
--
-- probably want nf for all of these benchmarks
-- http://www.serpentine.com/criterion/tutorial.html#benchmarking-pure-functions
-- given that I have a complex object returned at the end and I want more than just the outermost constructor
main :: IO ()
main =
  -- TODO: use env (https://hackage.haskell.org/package/criterion-1.6.3.0/docs/Criterion-Main.html#g:6)
  -- TODO: set a min number of... iters, so the larger runs still spend some time
  -- TODO: wire up scripts/bench to CI and put the artifact somewhere
  do
    jsonLines <- BS.readFile "intervals.json"
    intervals <- throwDecode' jsonLines :: IO [BogusData]
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
