{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.Instances where

import Data.Interval (Interval, IntervalLit (..))
import Data.IntervalIndex (IntervalIndex, fromList)
import Data.Monoid (Endo (..))
import Test.QuickCheck (Arbitrary (..), choose)

instance (Enum a, Arbitrary a) => Arbitrary (IntervalLit a) where
  arbitrary = do
    howLong <- choose (1, 5)
    open <- arbitrary
    let chooseSuccessor = mconcat . (Endo <$>) $ replicate howLong succ
    let close = appEndo chooseSuccessor open
    pure $ IntervalLit open close

instance (Arbitrary a, Interval k a) => Arbitrary (IntervalIndex k a) where
  arbitrary = fromList <$> arbitrary
