-- | Some specialized functions can be found here.

module Data.Traversable.Extra where

import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, traverse)

-- | Map an array conditionally, only return the array when all elements were mapped.
-- | Note that this function is an alias for `traverse`. This is specific behavior for the implementation of `<*>` for `Applicative Maybe`.
-- |
-- | Hint: mapAny can be found in Data.Foldable.Extra
-- |
-- | ```purescript
-- | mapAll (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Nothing
-- | mapAll (\x -> Just (x * 2)) [1,2,3] == Just [2,4,6]
-- | ```
mapAll :: forall a b f. Traversable f => (a -> Maybe b) -> f a -> Maybe (f b)
mapAll = traverse
