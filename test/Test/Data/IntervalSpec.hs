{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Data.IntervalSpec where

import Data.Interval (Interval (..), IntervalLit (..), covers, touches)
import Data.Foldable (traverse_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "intervals" $ do
  describe "point lookup" $ do
    it "finds points in ranges correctly" $ do
      (4 :: Int) `containedBy` IntervalLit 4 6 `shouldBe` True
      (6 :: Int) `containedBy` IntervalLit 4 6 `shouldBe` False
      (7 :: Int) `containedBy` IntervalLit 4 6 `shouldBe` False
  describe "range lookup" $
    let intervalLit1 :: IntervalLit Int
        intervalLit1 = IntervalLit 5 10
        intervalLit2 :: IntervalLit Int
        intervalLit2 = IntervalLit 7 12
        intervalLit3 :: IntervalLit Int
        intervalLit3 = IntervalLit 10 15
        intervalLit4 :: IntervalLit Int
        intervalLit4 = IntervalLit 0 15
        intervalLit5 :: IntervalLit Int
        intervalLit5 = IntervalLit 100 101
        intervalWrapper1 = IntervalWrapper (IntervalLit 'e' 'j', "one")
        intervalWrapper2 = IntervalWrapper (IntervalLit 'g' 'l', "two")
        intervalWrapper3 = IntervalWrapper (IntervalLit 'j' 'o', "three")
        intervalWrapper4 = IntervalWrapper (IntervalLit 'a' 'o', "four")
        intervalWrapper5 = IntervalWrapper (IntervalLit 'w' 'z', "five")
     in do
          touchesSuite "IntervalLit" intervalLit1 intervalLit2 intervalLit3 intervalLit4 intervalLit5
          touchesSuite
            "IntervalWrapper"
            intervalWrapper1
            intervalWrapper2
            intervalWrapper3
            intervalWrapper4
            intervalWrapper5
          coversSuite "IntervalLit" intervalLit1 intervalLit2 intervalLit3 intervalLit4 intervalLit5
          coversSuite "IntervalWrapper" intervalWrapper1 intervalWrapper2 intervalWrapper3 intervalWrapper4 intervalWrapper5

newtype IntervalWrapper = IntervalWrapper (IntervalLit Char, String)

instance Interval Char IntervalWrapper where
  intervalStart (IntervalWrapper (i, _)) = intervalStart i
  intervalEnd (IntervalWrapper (i, _)) = intervalEnd i

{- Test the touches relationship for five intervals arranged like:
 - interval 1:            ------
 - interval 2:              -------
 - interval 3:                 -------
 - interval 4: -----------------------
 - interval 5:                                ----
 - (right endpoints are open)
 -}
touchesSuite ::
  (Ord k, Interval k a) =>
  String ->
  a ->
  a ->
  a ->
  a ->
  a ->
  Spec
touchesSuite tag interval1 interval2 interval3 interval4 interval5 =
  it ("finds whether ranges touch correctly for " ++ tag) $
    do
      traverse_
        (\interval -> interval `touches` interval `shouldBe` True)
        [interval1, interval2, interval3, interval4, interval5]
      traverse_
        (\interval -> interval1 `touches` interval `shouldBe` True)
        [interval2, interval4]
      interval1 `touches` interval3 `shouldBe` False
      interval1 `touches` interval5 `shouldBe` False

{- Test the covers relationship for five intervals arranged like:
 - interval 1:            ------
 - interval 2:              -------
 - interval 3:                 -------
 - interval 4: -----------------------
 - interval 5:                                ----
 -}
coversSuite :: (Ord k, Interval k a) => String -> a -> a -> a -> a -> a -> Spec
coversSuite tag interval1 interval2 interval3 interval4 interval5 =
  it ("finds whether ranges cover correctly for " ++ tag) $
    do
      traverse_
        (\interval -> interval `covers` interval `shouldBe` True)
        [interval1, interval2, interval3, interval4, interval5]
      traverse_
        (\interval -> interval4 `covers` interval `shouldBe` True)
        [interval1, interval2, interval3]
      traverse_
        (\interval -> interval5 `covers` interval `shouldBe` False)
        [interval1, interval2, interval3, interval4]
