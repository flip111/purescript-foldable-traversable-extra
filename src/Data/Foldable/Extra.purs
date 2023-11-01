-- | Some specialized functions can be found here.

module Data.Foldable.Extra where

import Control.Applicative (class Applicative, pure)
import Data.Array.Extra.First (modifyOrSnoc)
import Data.Either (Either(..))
import Data.Eq (class Eq, (/=), (==))
import Data.Foldable (all, elem, length, null)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty, (<>))
import Data.Ord (class Ord)
import Data.Semiring ((+))
import Data.Traversable (class Foldable, foldl)
import Data.Tuple (Tuple(..))


-- | Try to make a projection from an array. Return an array with the projection and the original array with the elements removed.
-- |
-- | ```purescript
-- | partitionMaybe (\x -> if x == "dog" then Just "cat" else Nothing) ["apple", "dog", "kiwi"] == { no: ["apple", "kiwi"], yes: ["cat"] }
-- | ```
partitionMaybe :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> { no :: Array a, yes :: Array b }
partitionMaybe f xs = foldl go {yes: [], no: []} xs
  where go rec@{yes, no} x =
          case f x of
            Nothing -> rec {no  = no <> [x]}
            Just b  -> rec {yes = yes <> [b]}

-- | Map with a function that yields `Either`. Only succeeding when all elements where mapped to `Right`.
-- | Hint: if you don't care about collecting all the Left's (error conditions) and you are looking for a function like
-- | `forall a b c. (a -> Either c b) -> Array a -> Either c (Array b)` then use `traverse` from `Data.Traversable`.
mapEither :: forall a b c f. Foldable f => (a -> Either c b) -> f a -> Either (Array c) (Array b)
mapEither f foldable =
  let {lefts, rights} = foldl (g f) {lefts: [], rights: []} foldable
  in  if null lefts then
          Right rights
        else
          Left lefts
  where g :: forall q r s. (q -> Either s r) -> {lefts :: Array s, rights :: Array r} -> q -> {lefts :: Array s, rights :: Array r}
        g f2 {lefts, rights} elem = case f2 elem of
          Left l -> {lefts: lefts <> [l], rights}
          Right r -> {lefts, rights: rights <> [r]}

-- | Count the amount of times a value occurs in an array.
-- | Requires an Ord instance for Map. This function should be faster than `occurrences`
-- |
-- | ```purescript
-- | occurrencesMap ["A", "B", "B"] == Map.fromList [Tuple "A" 1, Tuple "B" 2]
-- | ```
occurrencesMap :: forall a f. Foldable f => Ord a => f a -> Map a Int
occurrencesMap xs = foldl go Map.empty xs
  where go acc x = Map.insertWith (\old _ -> old + 1) x 1 acc

-- | Count the amount of times a value occurs in an array.
-- | Mostly useful for when you can not define an Ord instance
-- |
-- | ```purescript
-- | occurrences ["A", "B", "B"] == [Tuple "A" 1, Tuple "B" 2]
-- | ```
occurrences :: forall a f. Eq a => Foldable f => f a -> Array (Tuple a Int)
occurrences xs = foldl go [] xs
  where go acc x = modifyOrSnoc (\(Tuple k _) -> k == x) (\(Tuple k v) -> Tuple k (v + 1)) acc (Tuple x 1)

-- | Checks if two arrays have exactly the same elements.
-- | The order of elements does not matter.
-- |
-- | ```purescript
-- | sameElements ["A", "B", "B"] ["B", "A", "B"] == true
-- | sameElements ["A", "B", "B"] ["A", "B"] == false
-- | ```
sameElements :: forall a f. Foldable f => Eq a => f a -> f a -> Boolean
sameElements a b =
  let l_a = length a :: Int
      l_b = length b :: Int
  in  if l_a /= l_b then false else
        let occ_a = occurrences a
            occ_b = occurrences b
            go :: Tuple a Int -> Boolean
            go x = x `elem` occ_b
        in  all go occ_a

-- | Map an array conditionally, only return the array when at least one element was mapped.
-- | Elements that are not mapped will keep the old value.
-- |
-- | Hint: mapAll can be found in Data.Traversable.Extra
-- |
-- | ```purescript
-- | mapAny (\_ -> Nothing) [1,2,3] == Nothing
-- | mapAny (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Just [1,99,3]
-- | ```
mapAny :: forall a f. Applicative f => Foldable f => Monoid (f a) => (a -> Maybe a) -> f a -> Maybe (f a)
mapAny f xs =
  let go (Tuple acc replaced) x = case f x of
          Nothing -> Tuple (acc <> pure x) replaced
          Just y  -> Tuple (acc <> pure y) true

      Tuple acc replaced = foldl go (Tuple mempty false) xs

  in  if replaced then
        Just acc
      else
        Nothing
