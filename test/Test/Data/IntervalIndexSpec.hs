module Test.Data.IntervalIndexSpec where

import Data.Interval (IntervalLit (..))
import Data.IntervalIndex
  ( at,
    findCoveringInterval,
    insert,
    touching,
  )
import qualified Data.IntervalIndex as IntervalIndex
import Data.IntervalIndex.Internal (IntervalIndex (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- TODO:
-- `merge`, and `delete`
spec :: Spec
spec = describe "interval indexs" $ do
  describe "construction" $ do
    it "builds from a list singleton" $
      IntervalIndex.fromList [IntervalLit 'a' 'f']
        `shouldBe` IntervalIndex
          { intervals = Map.singleton 0 (IntervalLit 'a' 'f'),
            index = Vector.fromList [IntervalLit 'a' 'f'],
            idMap = Map.singleton (IntervalLit 'a' 'f') (Set.singleton 0)
          }
    it "builds from a two-element list of non-overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'h' 'j'
       in IntervalIndex.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalIndex
              { index = Vector.fromList [firstInterval, secondInterval],
                intervals = Map.fromList [(0, firstInterval), (1, secondInterval)],
                idMap = Map.fromList [(firstInterval, Set.singleton 0), (secondInterval, Set.singleton 1)]
              }
    it "builds from a two-element list of overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'e' 'k'
       in IntervalIndex.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalIndex
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
        oneElementIntervalIndex `at` 'b' `shouldBe` [IntervalLit 'a' 'f']
        twoElementIntervalIndex `at` 'a' `shouldBe` [IntervalLit 'a' 'f']
        twoElementIntervalIndex `at` 'h' `shouldBe` [IntervalLit 'h' 'k']
      it "finds all intervals at points" $ do
        overlappingRangesIntervalIndex `at` 'a' `shouldBe` [IntervalLit 'a' 'j']
        overlappingRangesIntervalIndex `at` 'b' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
        overlappingRangesIntervalIndex `at` 'i' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
        overlappingRangesIntervalIndex `at` 'j' `shouldBe` [IntervalLit 'b' 'k']
      it "doesn't find intervals at points that aren't included" $ do
        oneElementIntervalIndex `at` 'f' `shouldBe` []
        oneElementIntervalIndex `at` 'g' `shouldBe` []
        twoElementIntervalIndex `at` 'g' `shouldBe` []
        overlappingRangesIntervalIndex `at` 'k' `shouldBe` []
  describe "querying at a range" $ do
    it "finds intervals over ranges" $ do
      oneElementIntervalIndex `touching` IntervalLit 'f' 'g' `shouldBe` []
      oneElementIntervalIndex `touching` IntervalLit 'g' 'h' `shouldBe` []
      oneElementIntervalIndex `touching` IntervalLit 'a' 'b' `shouldBe` [IntervalLit 'a' 'f']
      oneElementIntervalIndex `touching` IntervalLit 'a' 'f' `shouldBe` [IntervalLit 'a' 'f']
      twoElementIntervalIndex `touching` IntervalLit 'f' 'h'
        `shouldBe` []
      twoElementIntervalIndex `touching` IntervalLit 'a' 'k'
        `shouldBe` [IntervalLit 'a' 'f', IntervalLit 'h' 'k']
      twoElementIntervalIndex `touching` IntervalLit 'a' 'f'
        `shouldBe` [IntervalLit 'a' 'f']
      twoElementIntervalIndex `touching` IntervalLit 'h' 'j'
        `shouldBe` [IntervalLit 'h' 'k']
  describe "inserting values" $ do
    it "matches singleton for inserts into empty indices" $ do
      IntervalIndex.empty `insert` IntervalLit 'a' 'k'
        `shouldBe` IntervalIndex.singleton (IntervalLit 'a' 'k')
      IntervalIndex.empty `insert` IntervalLit False True
        `shouldBe` IntervalIndex.singleton (IntervalLit False True)
    it "matches fromList for inserting intervals into non-empty indices" $ do
      insertMatchesFromListTest [IntervalLit 'a' 'e', IntervalLit 'e' 'h']
      insertMatchesFromListTest [IntervalLit 'a' 'e', IntervalLit 'e' 'h', IntervalLit 'j' 'm']
      insertMatchesFromListTest [IntervalLit 'a' 'e', IntervalLit 'c' 'f']
      insertMatchesFromListTest [IntervalLit 'a' 'e', IntervalLit 'c' 'g', IntervalLit 'e' 'j']
      insertMatchesFromListTest [IntervalLit 'a' 'e', IntervalLit 'c' 'g', IntervalLit 'e' 'j', IntervalLit 'i' 'm']
      insertMatchesFromListTest [IntervalLit 'e' 'j', IntervalLit 'h' 'm', IntervalLit 'a' 'c']
      insertMatchesFromListTest [IntervalLit 'e' 'j', IntervalLit 'h' 'm', IntervalLit 'm' 'n']
      insertMatchesFromListTest [IntervalLit 'e' 'j', IntervalLit 'h' 'm', IntervalLit 'n' 'p']
      insertMatchesFromListTest [IntervalLit 'e' 'j', IntervalLit 'h' 'm', IntervalLit 'c' 'e']
      insertMatchesFromListTest [IntervalLit 'e' 'j', IntervalLit 'h' 'm', IntervalLit 'a' 'c']
  describe "merging indices" $ do
    it "gives back an empty index with two empty indices" pass
    it "treats an empty index as a left identity" pass
    it "treats an empty index as a right identity" pass
    it "reindexes the affected range when inserting into a populated index" pass
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

oneElementIntervalIndex :: IntervalIndex Char (IntervalLit Char)
oneElementIntervalIndex = IntervalIndex.fromList [IntervalLit 'a' 'f']

twoElementIntervalIndex :: IntervalIndex Char (IntervalLit Char)
twoElementIntervalIndex = IntervalIndex.fromList [IntervalLit 'a' 'f', IntervalLit 'h' 'k']

overlappingRangesIntervalIndex :: IntervalIndex Char (IntervalLit Char)
overlappingRangesIntervalIndex = IntervalIndex.fromList [IntervalLit 'a' 'j', IntervalLit 'b' 'k']

insertMatchesFromListTest :: [IntervalLit Char] -> Expectation
insertMatchesFromListTest intervalLits =
  IntervalIndex.fromList (take (length intervalLits - 1) intervalLits) `IntervalIndex.insert` last intervalLits
    `shouldBe` IntervalIndex.fromList intervalLits
