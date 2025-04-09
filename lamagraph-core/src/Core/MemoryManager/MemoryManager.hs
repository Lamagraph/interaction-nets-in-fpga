{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}

module Core.MemoryManager.MemoryManager (
  MemoryManager (..),
  busyBitMap,
  activePairs,
  ActivePair (..),
  leftNode,
  rightNode,
  indexToUnsigned,
  markAddress,
  giveActiveAddressNumberNoSignal,
  removeActivePairNoSignal,
  markAddressesAsBusyNoSignal,
  giveAddressesNoSignal,
) where

import Clash.Prelude
import Control.Lens hiding (Index, imap)
import Core.Node

{- $setup
>>> import Clash.Prelude
>>> import Core.Node
>>> import Core.Map
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

data MemoryManager (cellsNumber :: Nat)
  = MemoryManager
  { _busyBitMap :: Vec cellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _activePairs :: Vec cellsNumber Bool
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
getFreeAddressesNoSignal ::
  forall maxAddressesCount cellsNumber.
  ( KnownNat cellsNumber
  , KnownNat maxAddressesCount
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , 1 <= cellsNumber
  , maxAddressesCount <= cellsNumber
  ) =>
  Index cellsNumber ->
  Vec cellsNumber Bool ->
  Vec maxAddressesCount (Maybe AddressNumber)
getFreeAddressesNoSignal addressesCount busyMap = if addressesCount == 0 then def else helper 0 0 busyMap def
 where
  helper ::
    Index cellsNumber ->
    Index cellsNumber ->
    Vec m Bool ->
    Vec maxAddressesCount (Maybe AddressNumber) ->
    Vec maxAddressesCount (Maybe AddressNumber)
  helper allocatedCount busyMapIndex busyMapRemind addresses = case busyMapRemind of
    Nil -> error "Memory space is over"
    Cons isBusy remind ->
      if not isBusy
        then
          ( if (1 + allocatedCount) == addressesCount
              then
                Just (indexToUnsigned busyMapIndex) +>> addresses
              else
                helper (allocatedCount + 1) (busyMapIndex + 1) remind (Just (indexToUnsigned busyMapIndex) +>> addresses)
          )
        else
          helper allocatedCount (busyMapIndex + 1) remind addresses

-- | Mark given `AddressNumber`s as busy in busy map
markAddressesAsBusyNoSignal ::
  (KnownNat cellsNumber, KnownNat n, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber) =>
  Vec cellsNumber Bool ->
  Vec n (Maybe AddressNumber) ->
  Vec cellsNumber Bool
markAddressesAsBusyNoSignal busyMap addresses = imap (\i x -> Just (indexToUnsigned i) `elem` addresses || x) busyMap

giveAddressesNoSignal ::
  ( 1 <= cellsNumber
  , KnownNat cellsNumber
  , KnownNat maxAddressesCount
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , maxAddressesCount <= cellsNumber
  ) =>
  Index cellsNumber ->
  MemoryManager cellsNumber ->
  (Vec maxAddressesCount (Maybe AddressNumber), MemoryManager cellsNumber)
giveAddressesNoSignal addressesCount memoryManager = (addresses, set busyBitMap newBusyMap memoryManager)
 where
  addresses = getFreeAddressesNoSignal addressesCount (view busyBitMap memoryManager)
  newBusyMap = markAddressesAsBusyNoSignal (view busyBitMap memoryManager) addresses

{- | Mark given `AddressNumber` as busy or not according to passed flag (`True` means busy)

==== __Example__

>>> markAddress (repeat False :: Vec 4 Bool) True 2
False :> False :> True :> False :> Nil
-}
markAddress ::
  (KnownNat cellsNumber) =>
  Vec cellsNumber Bool ->
  Bool ->
  ActualAddressNumber ->
  Vec cellsNumber Bool
markAddress busyMap marker address =
  replace address marker busyMap

-- | Replace processed active pair at `False` in `Vec` of active pairs
deleteActivePair ::
  (KnownNat cellsNumber, KnownNat portsNumber) =>
  Vec cellsNumber Bool ->
  ActivePair portsNumber agentType ->
  Vec cellsNumber Bool
deleteActivePair oldActivePairs activePairToDelete =
  if leftInVec `xor` rightInVec
    then newActivePairs
    else error "In active pairs map should be exact one address" -- TODO: add link to the docs
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
  (KnownNat cellsNumber, KnownNat portsNumber) =>
  Vec cellsNumber Bool ->
  ActivePair portsNumber agentType ->
  Vec cellsNumber Bool
freeUpActivePair busyMap activePairToFree = markAddress (markAddress busyMap False leftNodeAddress) False rightNodeAddress
 where
  chooseAddress choice = activePairToFree ^. choice . originalAddress
  leftNodeAddress = chooseAddress leftNode
  rightNodeAddress = chooseAddress rightNode

-- | Remove all information about `ActivePair` from `MemoryManager`
removeActivePairNoSignal ::
  (KnownNat cellsNumber, KnownNat portsNumber) =>
  ActivePair portsNumber agentType ->
  MemoryManager cellsNumber ->
  MemoryManager cellsNumber
removeActivePairNoSignal acPair memoryManager = MemoryManager{..}
 where
  _activePairs = deleteActivePair (view activePairs memoryManager) acPair
  _busyBitMap = freeUpActivePair (view busyBitMap memoryManager) acPair

-- | Give `AddressNumber` of some active `Node`. It returns `Nothing` if there is no `ActivePair`s in the net
giveActiveAddressNumberNoSignal ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber) =>
  MemoryManager cellsNumber ->
  Maybe AddressNumber
giveActiveAddressNumberNoSignal memoryManager = fmap indexToUnsigned maybeIndexAddress
 where
  activePairsBusyMap = view activePairs memoryManager
  maybeIndexAddress = findIndex id activePairsBusyMap
