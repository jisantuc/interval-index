module Test.Data.IntervalTreeSpec where

import Data.Interval (IntervalLit (..))
import Data.IntervalTree (IntervalTree (..), at, findCoveringInterval, touching)
import qualified Data.IntervalTree as IntervalTree
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- TODO:
-- `insert`, `merge`, and `delete`
spec :: Spec
spec = describe "interval trees" $ do
  describe "construction" $ do
    it "builds from a list singleton" $
      IntervalTree.fromList [IntervalLit 'a' 'f']
        `shouldBe` IntervalTree
          { intervals = Map.singleton 0 (IntervalLit 'a' 'f'),
            index = Vector.fromList [IntervalLit 'a' 'f'],
            idMap = Map.singleton (IntervalLit 'a' 'f') (Set.singleton 0)
          }
    it "builds from a two-element list of non-overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'h' 'j'
       in IntervalTree.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalTree
              { index = Vector.fromList [firstInterval, secondInterval],
                intervals = Map.fromList [(0, firstInterval), (1, secondInterval)],
                idMap = Map.fromList [(firstInterval, Set.singleton 0), (secondInterval, Set.singleton 1)]
              }
    it "builds from a two-element list of overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'e' 'k'
       in IntervalTree.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalTree
              { index = Vector.fromList [IntervalLit 'a' 'e', IntervalLit 'e' 'f', IntervalLit 'f' 'k'],
                idMap =
                  Map.fromList
                    [ (IntervalLit 'a' 'e', Set.singleton 0),
                      (IntervalLit 'e' 'f', Set.fromList [0, 1]),
                      (IntervalLit 'f' 'k', Set.singleton 1)
                    ],
                intervals =
                  Map.fromList
                    [ (0, firstInterval),
                      (1, secondInterval)
                    ]
              }
  describe "querying at a point" $
    do
      it "finds intervals at points" $ do
        oneElementIntervalTree `at` 'b' `shouldBe` [IntervalLit 'a' 'f']
        twoElementIntervalTree `at` 'a' `shouldBe` [IntervalLit 'a' 'f']
        twoElementIntervalTree `at` 'h' `shouldBe` [IntervalLit 'h' 'k']
      it "finds all intervals at points" $ do
        overlappingRangesIntervalTree `at` 'a' `shouldBe` [IntervalLit 'a' 'j']
        overlappingRangesIntervalTree `at` 'b' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
        overlappingRangesIntervalTree `at` 'i' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
        overlappingRangesIntervalTree `at` 'j' `shouldBe` [IntervalLit 'b' 'k']
      it "doesn't find intervals at points that aren't included" $ do
        oneElementIntervalTree `at` 'f' `shouldBe` []
        oneElementIntervalTree `at` 'g' `shouldBe` []
        twoElementIntervalTree `at` 'g' `shouldBe` []
        overlappingRangesIntervalTree `at` 'k' `shouldBe` []
  describe "querying at a range" $ do
    it "finds intervals over ranges" $ do
      oneElementIntervalTree `touching` IntervalLit 'f' 'g' `shouldBe` []
      oneElementIntervalTree `touching` IntervalLit 'g' 'h' `shouldBe` []
      oneElementIntervalTree `touching` IntervalLit 'a' 'b' `shouldBe` [IntervalLit 'a' 'f']
      oneElementIntervalTree `touching` IntervalLit 'a' 'f' `shouldBe` [IntervalLit 'a' 'f']
      twoElementIntervalTree `touching` IntervalLit 'f' 'h'
        `shouldBe` []
      twoElementIntervalTree `touching` IntervalLit 'a' 'k'
        `shouldBe` [IntervalLit 'a' 'f', IntervalLit 'h' 'k']
      twoElementIntervalTree `touching` IntervalLit 'a' 'f'
        `shouldBe` [IntervalLit 'a' 'f']
      twoElementIntervalTree `touching` IntervalLit 'h' 'j'
        `shouldBe` [IntervalLit 'h' 'k']
  describe "finding the right key" $ do
    it "doesn't find a key when not present or in an empty list" $ do
      findCoveringInterval (Vector.fromList []) 'a' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'a' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'e' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'b' `shouldBe` Just (IntervalLit 'b' 'e')
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e', IntervalLit 'e' 'k', IntervalLit 'm' 'p']) 'l'
        `shouldBe` Nothing
      findCoveringInterval
        ( Vector.fromList
            [ IntervalLit 'b' 'e',
              IntervalLit 'e' 'k',
              IntervalLit 'm' 'p',
              IntervalLit 'p' 'q',
              IntervalLit 'r' 't'
            ]
        )
        'l'
        `shouldBe` Nothing
    it "finds a key when present in any size list" $ do
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'c' `shouldBe` Just (IntervalLit 'b' 'e')
      findCoveringInterval (Vector.fromList [IntervalLit 'a' 'd', IntervalLit 'f' 'g', IntervalLit 'g' 'i']) 'c'
        `shouldBe` Just (IntervalLit 'a' 'd')
      findCoveringInterval (Vector.fromList [IntervalLit 'a' 'd', IntervalLit 'f' 'g', IntervalLit 'g' 'i']) 'h'
        `shouldBe` Just (IntervalLit 'g' 'i')
      findCoveringInterval (Vector.fromList ((\c -> IntervalLit (pred c) c) <$> ['b' .. 'z'])) 'm'
        `shouldBe` Just (IntervalLit 'm' 'n')

pass :: Expectation
pass = () `shouldBe` ()

oneElementIntervalTree :: IntervalTree Char (IntervalLit Char)
oneElementIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'f']

twoElementIntervalTree :: IntervalTree Char (IntervalLit Char)
twoElementIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'f', IntervalLit 'h' 'k']

overlappingRangesIntervalTree :: IntervalTree Char (IntervalLit Char)
overlappingRangesIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
