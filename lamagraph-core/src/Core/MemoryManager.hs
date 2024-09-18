-- Just to pass CI, updateMM is not implement yet
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Core.MemoryManager where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))
import Core.Node
import Core.Reducer

data MemoryManager (cellsNumber :: Nat) = MemoryManager
  { _busyBitMap :: Vec cellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _visitedBitMap :: Vec cellsNumber Bool -- map of Address : Bool. tell smth like "Node by this address has primary port visited"
  , _nextAddressToHandle :: Maybe Address -- next Node which has no primary port visited
  }
  deriving (Show, Eq, Generic, NFDataX)

$(makeLenses ''MemoryManager)

data Delta (cellsNumber :: Nat) (portsNumber :: Nat) = Delta
  { _appearedNodes :: Vec cellsNumber (Maybe (Node portsNumber))
  , _deletedNodes :: Vec cellsNumber (Maybe (LoadedNode portsNumber))
  , _handledEdge :: Edge -- edge by that happened reduction
  }
  deriving (Show, Eq, Generic, NFDataX)

-- This is strange.
getUnUsedAddress ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber ~ 16) => MemoryManager cellsNumber -> Maybe Address
getUnUsedAddress memoryManager = address
 where
  indexOfUnused = elemIndex False (memoryManager ^. busyBitMap)
  address = bitCoerce <$> indexOfUnused :: Maybe Address

updateMM ::
  (KnownNat cellsNumber, KnownNat portsNumber) =>
  MemoryManager cellsNumber ->
  Delta cellsNumber portsNumber ->
  MemoryManager cellsNumber
updateMM memoryManager delta =
  undefined
