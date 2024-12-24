{-# LANGUAGE NamedFieldPuns #-}

module Data.IntervalIndex
  ( empty,
    singleton,
    fromList,
    at,
    insert,
    merge,
    touching,
    IntervalIndex,
    findCoveringInterval,
    allIntervals,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (fold, foldMap', foldl')
import Data.Functor ((<&>))
import Data.Interval
  ( Interval,
    IntervalLit (..),
    asEndpointVector,
    contains,
    intervalEnd,
    intervalStart,
    ofInterval,
    touches,
  )
import qualified Data.Interval as Interval
import Data.IntervalIndex.Internal (IntervalIndex (..))
import Data.List (sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as Vector

singleton :: (Interval k a) => a -> IntervalIndex k a
singleton = (`singletonWithId` 0)

singletonWithId :: (Interval k a) => a -> Integer -> IntervalIndex k a
singletonWithId a intervalId =
  IntervalIndex
    { index = Vector.singleton . ofInterval $ a,
      idMap = Map.singleton (ofInterval a) (Set.singleton intervalId),
      intervals = Map.singleton intervalId a
    }

empty :: (Interval k a, Ord k) => IntervalIndex k a
empty = IntervalIndex mempty mempty mempty

at :: (Interval k a) => IntervalIndex k a -> k -> [a]
at (IntervalIndex {index, idMap, intervals}) key =
  let coveringInterval = findCoveringInterval index key
      idsForInterval = coveringInterval >>= (`Map.lookup` idMap)
   in case idsForInterval of
        Nothing -> []
        Just intervalIds -> intervals `forIds` intervalIds

insert :: (Interval k a) => IntervalIndex k a -> a -> IntervalIndex k a
insert idx@(IntervalIndex {index, idMap, intervals}) value
  | Prelude.null index = singleton value
  | Interval.null value = idx
  | otherwise =
      let keyForNewValue = ofInterval value
          maxId = maximum $ Map.keys intervals
          newIntervalId = maxId + 1
          newIntervals = Map.insert newIntervalId value intervals
          keyContainingStart = findCoveringInterval index (intervalStart value)
          keyContainingEnd = findCoveringInterval index (intervalEnd value)
          firstEndpoint = maybe (intervalStart value) start keyContainingStart
          lastEndpoint = maybe (intervalEnd value) end keyContainingEnd
          withoutPrefix = Vector.dropWhile (\intervalLit -> end intervalLit <= firstEndpoint) index
          keysToRebuild = Vector.takeWhile (\intervalLit -> end intervalLit <= lastEndpoint) withoutPrefix
          endpointsToRebuild =
            sort . nubOrd $
              ( [intervalStart value, intervalEnd value]
                  ++ Vector.toList (keysToRebuild >>= asEndpointVector)
              )
          keyRangeToRebuild = Vector.fromList $ zipWith IntervalLit endpointsToRebuild (tail endpointsToRebuild)
          (newIndex, newIdMap) =
            if Prelude.null keyRangeToRebuild
              then
                let (h, t) = Vector.span (\intervalLit -> end intervalLit <= start keyForNewValue) index
                 in ( Vector.concat [h, Vector.singleton keyForNewValue, t],
                      Map.insert keyForNewValue (Set.singleton newIntervalId) idMap
                    )
              else
                let minStart = start . Vector.head $ keyRangeToRebuild
                    maxEnd = end . Vector.last $ keyRangeToRebuild
                    h = Vector.takeWhile (\IntervalLit {end} -> end <= minStart) index
                    t = Vector.dropWhile (\IntervalLit {start} -> start < maxEnd) index
                    withoutNewIndex =
                      Map.filterWithKey
                        (\(IntervalLit {start, end}) _ -> start >= maxEnd || end <= minStart)
                        idMap
                    updatedMap =
                      Map.fromList . Vector.toList $
                        keyRangeToRebuild <&> \intervalLit ->
                          let coveringKey = findCoveringInterval index (start intervalLit)
                              originalIds = fold $ coveringKey >>= (`Map.lookup` idMap)
                           in if intervalLit `touches` ofInterval value
                                then
                                  ( intervalLit,
                                    originalIds `Set.union` Set.singleton newIntervalId
                                  )
                                else (intervalLit, originalIds)
                 in ( Vector.concat [h, keyRangeToRebuild, t],
                      Map.union withoutNewIndex updatedMap
                    )
       in IntervalIndex {index = newIndex, idMap = newIdMap, intervals = newIntervals}

touchingKeys :: (Interval k a) => IntervalIndex k a -> IntervalLit k -> Vector.Vector (IntervalLit k)
touchingKeys (IntervalIndex {index}) range = fromMaybe Vector.empty $ do
  firstIndex <- Vector.findIndex (range `touches`) index
  lastIndex <- Vector.findIndexR (range `touches`) index
  pure $ Vector.slice firstIndex (lastIndex - firstIndex + 1) index

touching :: (Interval k a) => IntervalIndex k a -> IntervalLit k -> [a]
touching idx@(IntervalIndex {idMap, intervals}) range =
  let indices = Vector.toList $ touchingKeys idx range
      ids = fromMaybe Set.empty $ foldMap' (`Map.lookup` idMap) indices
   in intervals `forIds` ids

merge :: (Interval k a) => IntervalIndex k a -> IntervalIndex k a -> IntervalIndex k a
merge idx1 idx2
  | Vector.null $ index idx1 = idx2
  | Vector.null $ index idx2 = idx1
  -- this is needlessly slow and risks rebuilding the index a whole bunch of times vs. finding the affected range
  -- for the collection of keys and rebuilding that index range once, _but_ I can write tests against it because
  -- it Does the Right Thing™️, which is useful for _the future_ when I want to make it faster.
  | otherwise = foldl' insert idx1 (Map.elems . intervals $ idx2)

fromList :: (Ord k, Interval k a) => [a] -> IntervalIndex k a
fromList [] = empty
fromList [x] = singleton x
fromList intervalData = fromListWithIds intervalData [0 ..]

fromListWithIds :: (Ord k, Interval k a) => [a] -> [Integer] -> IntervalIndex k a
fromListWithIds [] _ = empty
fromListWithIds [x] [integerId] = singletonWithId x integerId
fromListWithIds xs ids =
  let allEndpoints = xs >>= (\i -> [intervalStart i, intervalEnd i])
      uniqueEndpoints = sort . nubOrd $ allEndpoints
      fullIndex = zipWith IntervalLit uniqueEndpoints (tail uniqueEndpoints)
      zipped = zip ids xs
      sortedIntervals = sortBy (\(_, i1) (_, i2) -> compare (intervalStart i1) (intervalStart i2)) zipped
      intervals = Map.fromList sortedIntervals
      mergeMaps = foldl' (Map.unionWithKey (const mappend)) mempty
      idMap =
        mergeMaps $
          sortedIntervals
            >>= ( \(intervalId, interval) ->
                    let endpoints =
                          takeWhile (\key -> key <= intervalEnd interval) $
                            dropWhile (\key -> key < intervalStart interval) uniqueEndpoints
                        withTail = zip endpoints (tail endpoints)
                     in (\(s, e) -> Map.singleton (IntervalLit s e) (Set.singleton intervalId)) <$> withTail
                )
      index =
        Vector.fromList $
          filter
            (`Map.member` idMap)
            fullIndex
   in IntervalIndex {index, idMap, intervals}

-- | Find the covering interval in sorted, non-overlapping interval lits.
findCoveringInterval :: (Ord k) => Vector.Vector (IntervalLit k) -> k -> Maybe (IntervalLit k)
findCoveringInterval xs k =
  case Vector.uncons xs of
    Nothing -> Nothing
    Just (h, t) | Vector.null t && h `contains` k -> Just h
    Just (h, t) | Vector.null t && not (h `contains` k) -> Nothing
    _ ->
      ( let (midpoint, _) = length xs `divMod` 2
            splitPoint = midpoint
            interval@(IntervalLit s e) = xs Vector.! splitPoint
            (left, right) = Vector.splitAt splitPoint xs
         in case (compare s k, compare e k) of
              (LT, GT) -> Just interval
              (EQ, _) -> Just interval
              (_, EQ) -> findCoveringInterval right k
              (_, LT) -> findCoveringInterval right k
              (GT, _) -> findCoveringInterval left k
      )

forIds :: Map.Map Integer a -> Set.Set Integer -> [a]
forIds intervals ids =
  let idList = Set.toList ids
   in mapMaybe (`Map.lookup` intervals) idList

allIntervals :: IntervalIndex k a -> [a]
allIntervals (IntervalIndex {intervals}) = Map.elems intervals

instance (Interval k a) => Semigroup (IntervalIndex k a) where
  (<>) = merge

instance (Interval k a) => Monoid (IntervalIndex k a) where
  mempty = empty
