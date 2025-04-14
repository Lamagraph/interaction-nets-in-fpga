{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}

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

data ActivePair (portsNumber :: Nat) agentType = ActivePair
  { _leftNode :: LoadedNode portsNumber agentType
  , _rightNode :: LoadedNode portsNumber agentType
  }
  deriving (Show, Eq, Generic, NFDataX, Bundle, ShowX)
$(makeLenses ''ActivePair)

data MemoryManager
  = MemoryManager
  { _busyBitMap :: Vec CellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _activePairs :: Vec CellsNumber Bool
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

-- | Get `Vec` of free `AddressNumber`s of given size
getFreeAddresses ::
  forall maxAddressesCount.
  ( KnownNat maxAddressesCount
  ) =>
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

-- | Replace processed active pair at `False` in `Vec` of active pairs
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

-- | Mark `ActivePair`'s place as free
freeUpActivePair ::
  (KnownNat portsNumber) =>
  Vec CellsNumber Bool ->
  ActivePair portsNumber agentType ->
  Vec CellsNumber Bool
freeUpActivePair busyMap activePairToFree = markAddress (markAddress busyMap False leftNodeAddress) False rightNodeAddress
 where
  leftNodeAddress = activePairToFree ^. leftNode . originalAddress
  rightNodeAddress = activePairToFree ^. rightNode . originalAddress

-- | Remove all information about `ActivePair` from `MemoryManager`
removeActivePair ::
  (KnownNat portsNumber) =>
  ActivePair portsNumber agentType ->
  MemoryManager ->
  MemoryManager
removeActivePair acPair memoryManager = MemoryManager{..}
 where
  _activePairs = deleteActivePair (view activePairs memoryManager) acPair
  _busyBitMap = freeUpActivePair (view busyBitMap memoryManager) acPair

-- | Give `AddressNumber` of some active `Node`. It returns `Nothing` if there is no `ActivePair`s in the net
giveActiveAddressNumber ::
  MemoryManager ->
  Maybe AddressNumber
giveActiveAddressNumber MemoryManager{..} = fmap indexToUnsigned maybeIndexAddress
 where
  maybeIndexAddress = findIndex id _activePairs
