{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}

{- |
Module      : Core.MemoryManager.MemoryManager
Description : Occupancy bookkeeping for the 16-cell net memory

Maintains the two bit maps over all `CellsNumber` cell addresses:
`_busyBitMap` (a node lives here) and `_activePairs` (this address heads an
active pair), and provides the operations the CPU FSM and
"Core.Reducer" need: allocate free addresses for fresh nodes, find an
active address to fetch, and retire a reduced pair.

Invariant kept here: for an address marked in `_activePairs` the node stored
there is active; `deleteActivePair` aborts (`errorX`) unless exactly one of
the two addresses of the pair is marked.
-}
module Core.MemoryManager.MemoryManager where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, imap)
import Core.Node

{- $setup
>>> import Clash.Prelude
>>> import Core.Node
>>> import Control.Lens hiding (Index, ifoldl, imap, (:>))
>>> :set -XAllowAmbiguousTypes
>>> :set -fplugin GHC.TypeLits.Extra.Solver
>>> :set -fplugin GHC.TypeLits.KnownNat.Solver
>>> :set -fplugin GHC.TypeLits.Normalise
-}

{- | The two nodes of an interaction-redex, both loaded from RAM with their addresses.
Their principal ports are connected to each other.
-}
data ActivePair (portsNumber :: Nat) agentType = ActivePair
  { _leftNode :: LoadedNode portsNumber agentType
  , _rightNode :: LoadedNode portsNumber agentType
  }
  deriving (Show, Eq, Generic, NFDataX, Bundle, ShowX)

$(makeLenses ''ActivePair)

-- | Occupancy state of the cell memory (see the module header).
data MemoryManager
  = MemoryManager
  { _busyBitMap :: Vec CellsNumber Bool
  -- ^ @True@ = the address is busy, nothing can be written there
  , _activePairs :: Vec CellsNumber Bool
  -- ^ @True@ = the address holds the head of an active pair
  }
  deriving (Generic, NFDataX, Bundle, Show)

$(makeLenses ''MemoryManager)

{- | Cast `Index` to `Unsigned` if possible by adding non-significant zeros

==== __Example__

>>> indexToUnsigned (2 :: Index 4) :: Unsigned 16
2
-}
indexToUnsigned ::
  forall n m.
  (KnownNat n, KnownNat m, 1 <= n, CLog 2 n <= m) =>
  Index n ->
  Unsigned m
indexToUnsigned v = bitCoerce (resize v :: Index (2 ^ m))

{- | Get `Vec` of free `AddressNumber`s of given size.
Yields the lowest free addresses, front-packed in descending order (each one is
prepended as the busy map is scanned), padded with `Nothing`; when there are
fewer free addresses than requested the missing slots stay `Nothing` (the
caller — a reduction rule — sees an exhausted pool).
-}
getFreeAddresses ::
  forall maxAddressesCount.
  (KnownNat maxAddressesCount) =>
  Index CellsNumber ->
  Vec CellsNumber Bool ->
  Vec maxAddressesCount (Maybe AddressNumber)
getFreeAddresses addressesCount busyMap =
  if addressesCount == 0
    then def
    else
      snd $
        ifoldl
          ( \(allocatedCount, addresses) i isBusy ->
              ( if (allocatedCount < addressesCount) && not isBusy
                  then (allocatedCount + 1, Just (indexToUnsigned i) +>> addresses)
                  else (allocatedCount, addresses)
              )
          )
          (0, def)
          busyMap

-- | Mark given `AddressNumber`s as busy in busy map
markAddressesAsBusy ::
  (KnownNat n, 1 <= CellsNumber, CLog 2 CellsNumber <= BitSize AddressNumber) =>
  Vec CellsNumber Bool ->
  Vec n (Maybe AddressNumber) ->
  Vec CellsNumber Bool
markAddressesAsBusy busyMap addresses = imap (\address addressIsBusy -> Just (indexToUnsigned address) `elem` addresses || addressIsBusy) busyMap

{- | Take @addressesCount@ free addresses and mark them busy in one step.
Returns fewer (or zero) addresses, padded with `Nothing`, when the memory
holds fewer free cells than requested.
-}
giveAddresses ::
  (KnownNat maxAddressesCount) =>
  Index CellsNumber ->
  MemoryManager ->
  (Vec maxAddressesCount (Maybe AddressNumber), MemoryManager)
giveAddresses addressesCount mm@(MemoryManager busyMap _) = (addresses, mm{_busyBitMap = newBusyMap})
 where
  addresses = getFreeAddresses addressesCount busyMap
  newBusyMap = markAddressesAsBusy busyMap addresses

-- | Mark given `AddressNumber` as busy or not according to passed flag (`True` means busy)
markAddress ::
  Vec CellsNumber Bool ->
  Bool ->
  AddressNumber ->
  Vec CellsNumber Bool
markAddress busyMap marker address =
  replace address marker busyMap

{- | Mark the place of the reduced pair as free in the active-pairs map.

Enforces the \"exactly one node of the pair is marked\" invariant: if both or
neither address of the pair are marked in the vector, aborts with `errorX`.
-}
deleteActivePair ::
  (KnownNat portsNumber) =>
  Vec CellsNumber Bool ->
  ActivePair portsNumber agentType ->
  Vec CellsNumber Bool
deleteActivePair oldActivePairs activePairToDelete =
  if leftInVec `xor` rightInVec
    then newActivePairs
    else errorX "In active pairs map should be exact one address" -- TODO: add link to the docs
 where
  leftInVec = oldActivePairs !! (activePairToDelete ^. leftNode . originalAddress)
  rightInVec = oldActivePairs !! (activePairToDelete ^. rightNode . originalAddress)
  newActivePairs =
    if leftInVec
      then
        replace (activePairToDelete ^. leftNode . originalAddress) False oldActivePairs
      else
        replace (activePairToDelete ^. rightNode . originalAddress) False oldActivePairs

-- | Mark both addresses of the pair as free in the busy map (their cells may be reused by the rule's fresh nodes).
freeUpActivePair ::
  (KnownNat portsNumber) =>
  Vec CellsNumber Bool ->
  ActivePair portsNumber agentType ->
  Vec CellsNumber Bool
freeUpActivePair busyMap activePairToFree = markAddress (markAddress busyMap False leftNodeAddress) False rightNodeAddress
 where
  leftNodeAddress = activePairToFree ^. leftNode . originalAddress
  rightNodeAddress = activePairToFree ^. rightNode . originalAddress

-- | Remove all information about @ActivePair@ from @MemoryManager@
removeActivePair ::
  (KnownNat portsNumber) =>
  ActivePair portsNumber agentType ->
  MemoryManager ->
  MemoryManager
removeActivePair acPair memoryManager = MemoryManager{..}
 where
  _activePairs = deleteActivePair (view activePairs memoryManager) acPair
  _busyBitMap = freeUpActivePair (view busyBitMap memoryManager) acPair

-- | Give `AddressNumber` of some active @Node@ — the lowest marked address. It returns `Nothing` if there is no @ActivePair@s in the net.
giveActiveAddressNumber ::
  MemoryManager ->
  Maybe AddressNumber
giveActiveAddressNumber MemoryManager{..} = fmap indexToUnsigned maybeIndexAddress
 where
  maybeIndexAddress = findIndex id _activePairs
