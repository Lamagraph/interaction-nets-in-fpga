module Core.Map (Map, findByKey, changeValueByKey) where

import Clash.Prelude
import Core.Node

{- | key-value store based on `Vec`. Key is `AddressNumber`, it has linear time to find or change element.
But iterate (such as map or fold) by value and key is easy.
It assumed that `size` is small
-}
type Map (size :: Nat) v = (Vec size (Maybe (AddressNumber, Maybe v)))

findByKey ::
  (KnownNat size) =>
  Map size v ->
  AddressNumber ->
  Maybe v
findByKey dict key = case dict of
  Nil -> Nothing
  (Just (k, v) `Cons` t) -> if k == key then v else findByKey t key
  (Nothing `Cons` t) -> findByKey t key

-- | Update or insert (by applying function to `Nothing`) value by the key
changeValueByKey ::
  (KnownNat size, Eq v) =>
  Map size v ->
  (Maybe v -> Maybe v) ->
  AddressNumber ->
  Map size v
changeValueByKey dict func key = if dict /= updated then updated else inserted
 where
  updated = tryToUpdate key func dict
  inserted = insertInFree key (func Nothing) dict

-- | Try to update (by applying the function) value by key. It do nothing if there is no value be the key in the `Map`
tryToUpdate ::
  AddressNumber ->
  (Maybe v -> Maybe v) ->
  Map size v ->
  Map size v
tryToUpdate key func dict = case dict of
  h `Cons` t -> case h of
    Nothing -> h `Cons` tryToUpdate key func t
    Just (k, v) -> if k == key then Just (k, func v) `Cons` t else h `Cons` tryToUpdate key func t
  Nil -> Nil

-- | Insert key-value pair in the free space
insertInFree ::
  AddressNumber ->
  Maybe v ->
  Map size v ->
  Map size v
insertInFree key value dict = case dict of
  h `Cons` t -> case h of
    Nothing -> Just (key, value) `Cons` t
    Just _ -> h `Cons` insertInFree key value t
  Nil -> error "All addresses are written"
