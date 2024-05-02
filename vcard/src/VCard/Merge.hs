module VCard.Merge
  ( mergeList,
    mergeNE,
    mergeMaybe,
    mergeValue,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

-- NE.fromList here is safe because the resulting list cannot be empty
mergeNE :: (Ord a) => (c -> NonEmpty a) -> c -> c -> NonEmpty a
mergeNE func c1 c2 = NE.fromList $ mergeList (NE.toList . func) c1 c2

mergeList :: (Ord a) => (c -> [a]) -> c -> c -> [a]
mergeList func c1 c2 = nubOrd $ func c1 <> func c2

mergeMaybe :: (Eq a) => (c -> Maybe a) -> c -> c -> Maybe a
mergeMaybe func c1 c2 = case (func c1, func c2) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just a) -> Just a
  (Just a1, Just a2) -> Just $ mergeValue a1 a2

mergeValue :: (Eq a) => a -> a -> a
mergeValue a1 a2 =
  if a1 == a2
    then a1
    else a2
