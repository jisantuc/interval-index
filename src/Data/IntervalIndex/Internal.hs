module Data.IntervalIndex.Internal (IntervalIndex (..)) where

import Data.Interval (IntervalLit)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)

data IntervalIndex k a = IntervalIndex
  { index :: Vector (IntervalLit k),
    idMap :: Map (IntervalLit k) (Set Integer),
    intervals :: Map Integer a
  }
  deriving (Eq, Show)
