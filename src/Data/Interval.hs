{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Interval
  ( Interval (..),
    IntervalLit (..),
    ofInterval,
    asEndpointVector,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

-- TODO convert to multiline docstring

-- | Typeclass representing data covering some interval from `intervalStart` to `intervalEnd`,
--
-- where `intervalStart` and `intervalEnd` are comparable values calculated from some value of
-- type `a`.
class (Ord key) => Interval key a | a -> key where
  intervalStart :: a -> key
  intervalEnd :: a -> key

  contains :: a -> key -> Bool
  contains interval value =
    value >= intervalStart interval
      && value < intervalEnd interval

  containedBy :: key -> a -> Bool
  containedBy = flip contains

  covers :: a -> a -> Bool
  covers i1 i2 = intervalStart i1 <= intervalStart i2 && intervalEnd i1 >= intervalEnd i2

  coveredBy :: a -> a -> Bool
  coveredBy = flip covers

  touches :: a -> a -> Bool
  touches i1 i2 =
    -- if either interval covers the other, they touch
    i1 `covers` i2
      || i1 `coveredBy` i2
      -- or if interval 1 includes the start of interval 2
      || intervalStart i1 <= intervalStart i2 && intervalEnd i1 > intervalStart i2
      -- or if interval 1 includes the end of interval 2
      || intervalStart i1 < intervalEnd i2 && intervalEnd i1 >= intervalEnd i2

  null :: a -> Bool
  null value = intervalStart value == intervalEnd value

-- | A type of literal intervals that carry no data other than their start/stop
data IntervalLit a = IntervalLit
  { start :: a,
    end :: a
  }
  deriving (Eq, Generic, Ord, Show)

instance (NFData a) => NFData (IntervalLit a)

ofInterval :: (Interval k a) => a -> IntervalLit k
ofInterval x = IntervalLit (intervalStart x) (intervalEnd x)

asEndpointVector :: IntervalLit a -> Vector.Vector a
asEndpointVector (IntervalLit {start, end}) = Vector.fromList [start, end]

instance (Ord a) => Interval a (IntervalLit a) where
  intervalStart :: IntervalLit a -> a
  intervalStart = start
  intervalEnd :: IntervalLit a -> a
  intervalEnd = end
