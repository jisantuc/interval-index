{-# LANGUAGE DeriveGeneric #-}

module Data.IntervalIndex.Internal (IntervalIndex (..)) where

import Control.DeepSeq (NFData)
import Data.Interval (IntervalLit)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data IntervalIndex k a = IntervalIndex
  { index :: Vector (IntervalLit k),
    idMap :: Map (IntervalLit k) (Set Integer),
    intervals :: Map Integer a
  }
  deriving (Eq, Generic, Show)

instance (NFData a, NFData k) => NFData (IntervalIndex k a)
