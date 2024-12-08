{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}

module Core.MemoryManager.MemoryManager (
  MemoryManager (..),
  busyBitMap,
  activePairs,
  giveAddresses,
  removeActivePair,
) where

import Clash.Prelude
import Control.Lens hiding (Index, imap)
import Core.Node
import Core.Reducer

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
  forall addressesCount dom cellsNumber.
  ( (addressesCount + 1) <= cellsNumber
  , KnownNat cellsNumber
  , KnownNat addressesCount
  , KnownDomain dom
  , CLog 2 cellsNumber <= BitSize AddressNumber
  ) =>
  Signal dom (SNat addressesCount) ->
  Signal dom (Vec cellsNumber Bool) ->
  Signal dom (Vec addressesCount AddressNumber)
getFreeAddresses addressesCount busyMap =
  helper 0 0 (unbundle busyMap) def
 where
  helper ::
    Signal dom (Index cellsNumber) ->
    Index cellsNumber ->
    Vec m (Signal dom Bool) ->
    Vec addressesCount (Signal dom AddressNumber) ->
    Signal dom (Vec addressesCount AddressNumber)
  helper allocatedCount busyMapIndex busyMapRemind addresses = case busyMapRemind of
    Nil -> error "Memory space is over"
    Cons isBusy remind ->
      mux
        (not <$> isBusy)
        ( mux
            ((1 + allocatedCount) .==. (fromSNat <$> addressesCount :: Signal dom (Index cellsNumber)))
            (bundle (pure (indexToUnsigned busyMapIndex) +>> addresses))
            (helper (allocatedCount + 1) (busyMapIndex + 1) remind (pure (indexToUnsigned busyMapIndex) +>> addresses))
        )
        (helper allocatedCount (busyMapIndex + 1) remind addresses)

-- | Mark given `AddressNumber`s as busy in busy map
markAddressesAsBusy ::
  (KnownDomain dom, KnownNat cellsNumber, KnownNat n, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber) =>
  Signal dom (Vec cellsNumber Bool) ->
  Signal dom (Vec n AddressNumber) ->
  Signal dom (Vec cellsNumber Bool)
markAddressesAsBusy busyMap addresses = bundle $ imap (\i _ -> elem (indexToUnsigned i) <$> addresses) (unbundle busyMap)

data MemoryManager (cellsNumber :: Nat)
  = MemoryManager
  { _busyBitMap :: Vec cellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _activePairs :: Vec cellsNumber Bool
  }
  deriving (Generic, NFDataX, Bundle)

$(makeLenses ''MemoryManager)

giveAddresses ::
  ( (addressesCount + 1) <= cellsNumber
  , KnownNat cellsNumber
  , KnownNat addressesCount
  , KnownDomain dom
  , CLog 2 cellsNumber <= BitSize AddressNumber
  ) =>
  Signal dom (SNat addressesCount) ->
  Signal dom (MemoryManager cellsNumber) ->
  (Signal dom (Vec addressesCount AddressNumber), Signal dom (MemoryManager cellsNumber))
giveAddresses addressesCount memoryManager = (addresses, set busyBitMap <$> newBusyMap <*> memoryManager)
 where
  addresses = getFreeAddresses addressesCount (view busyBitMap <$> memoryManager)
  newBusyMap = markAddressesAsBusy (view busyBitMap <$> memoryManager) addresses

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
  ActivePair portsNumber ->
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
  ActivePair portsNumber ->
  Vec cellsNumber Bool
freeUpActivePair busyMap activePairToFree = markAddress (markAddress busyMap False leftNodeAddress) False rightNodeAddress
 where
  chooseAddress choice = activePairToFree ^. choice . originalAddress
  leftNodeAddress = chooseAddress leftNode
  rightNodeAddress = chooseAddress rightNode

-- | Remove all information about `ActivePair` from `MemoryManager`
removeActivePair ::
  (KnownNat cellsNumber, KnownNat portsNumber, KnownDomain dom) =>
  Signal dom (ActivePair portsNumber) ->
  Signal dom (MemoryManager cellsNumber) ->
  Signal dom (MemoryManager cellsNumber)
removeActivePair acPair memoryManager =
  set busyBitMap
    <$> newBusyMap
    <*> ( set activePairs
            <$> newActivePairs
            <*> memoryManager
        )
 where
  newActivePairs = deleteActivePair <$> (view activePairs <$> memoryManager) <*> acPair
  newBusyMap = freeUpActivePair <$> (view busyBitMap <$> memoryManager) <*> acPair