{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
Module      : Core.Map
Description : Tiny address-keyed association `Vec` for accumulated updates

A key-value store specialized for `AddressNumber` keys: a fixed-size `Vec` of
@Maybe (key, value)@ slots with linear search. Chosen over a tree because the
maps are tiny (at most one entry per external node of an active pair) and
every consumer folds over the whole structure. The only user is
"Core.MemoryManager.ChangesAccumulator", which collects @Changes@ per
external address.

`OPTIONS_HADDOCK ignore-exports` is set so the internal @update@ helper, which
is hidden by the export list but carries a written contract, still appears in
the generated documentation. The export list itself is unchanged.
-}
module Core.Map (Map, find, insertWith, insert) where

import Clash.Prelude
import Core.Node

{- $setup
>>> import Clash.Prelude
>>> import Core.Node
-}

{- | key-value store based on `Vec`. Key is `AddressNumber`, it has linear time to find or change element.
But iterate (such as map or fold) by value and key is easy.
It assumed that @size@ is small.
The inner `Maybe` of a slot value lets a slot be reserved as a key with no value yet.
-}
type Map (size :: Nat) v = (Vec size (Maybe (AddressNumber, Maybe v)))

{- | Value by key: the first slot whose key matches. Yields `Nothing` both when the
key is absent and when the slot is empty or holds a reserved key with no value.
-}
find ::
  (KnownNat size) =>
  Map size v ->
  AddressNumber ->
  Maybe v
find dict key = case dict of
  Nil -> Nothing
  (Just (k, v) `Cons` t) -> if k == key then v else find t key
  (Nothing `Cons` t) -> find t key

{- | Update or insert (by applying function to `Nothing`) value by the key.

The function is applied to the current slot value (@Nothing@ when the key is
absent or reserved) and written back into the matched slot; only when the key
was absent is a new slot taken via `insert` — into the first free slot.
If the map is full, `insert` aborts with @error "All addresses are written"@;
there is no silent truncation.

TODO: fix @id@ case. If @func@ is isomorphic to @id@ then @insertWith@ works incorrect:
an update that leaves the map bit-for-bit equal is mistaken for \"absent key\" and
the same key is inserted a second time (both slots then hold the same value, so
`find` stays correct but a slot is wasted).
-}
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

-- | Try to update (by applying the function) value by key. It does nothing (returns the map unchanged) if there is no value by the key in the `Map`
update ::
  AddressNumber ->
  (Maybe v -> Maybe v) ->
  Map size v ->
  Map size v
update key func dict = map (fmap updateIfKeysEquals) dict
 where
  updateIfKeysEquals (k, v) = if k == key then (k, func v) else (k, v)

{- | Insert key-value pair in the free space (first empty slot). Aborts with
@error "All addresses are written"@ when the map is full — overflow is an
explicit error, never silent truncation.

>>> insert 1 (Just 1) (def :: Map 2 AddressNumber)
Just (1,Just 1) :> Nothing :> Nil
-}
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
