module Core.Map (Map, find, insertWith) where

import Clash.Prelude
import Core.Node

{- | key-value store based on `Vec`. Key is `AddressNumber`, it has linear time to find or change element.
But iterate (such as map or fold) by value and key is easy.
It assumed that `size` is small
-}
type Map (size :: Nat) v = (Vec size (Maybe (AddressNumber, Maybe v)))

find ::
  (KnownNat size) =>
  Map size v ->
  AddressNumber ->
  Maybe v
find dict key = case dict of
  Nil -> Nothing
  (Just (k, v) `Cons` t) -> if k == key then v else find t key
  (Nothing `Cons` t) -> find t key

-- | Update or insert (by applying function to `Nothing`) value by the key
insertWith ::
  (KnownNat size, Eq v) =>
  Map size v ->
  (Maybe v -> Maybe v) ->
  AddressNumber ->
  Map size v
insertWith dict func key = if dict /= updated then updated else inserted
 where
  updated = update key func dict
  inserted = insert key (func Nothing) dict

-- | Try to update (by applying the function) value by key. It does nothing if there is no value by the key in the `Map`
update ::
  AddressNumber ->
  (Maybe v -> Maybe v) ->
  Map size v ->
  Map size v
update key func dict = case dict of
  h `Cons` t -> case h of
    Nothing -> h `Cons` update key func t
    Just (k, v) -> if k == key then Just (k, func v) `Cons` t else h `Cons` update key func t
  Nil -> Nil

-- | Insert key-value pair in the free space
insert ::
  AddressNumber ->
  Maybe v ->
  Map size v ->
  Map size v
insert key value dict = case dict of
  h `Cons` t -> case h of
    Nothing -> Just (key, value) `Cons` t
    Just _ -> h `Cons` insert key value t
  Nil -> error "All addresses are written"
