module Core.Loader where

import Clash.Prelude
import Control.Lens (view)
import Core.MemoryManager.MemoryManager (ActivePair (ActivePair), leftNode, rightNode)
import Core.MemoryManager.NodeChanges
import Core.Node

-- | Type alias for partial applied `blockRam`
type Ram dom portsNumber agentType =
  ( Signal dom AddressNumber ->
    Signal dom (Maybe (AddressNumber, Maybe (Node portsNumber agentType))) ->
    Signal dom (Maybe (Node portsNumber agentType))
  )

-- | Read external `Node`s from ram
loadInterface ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat portsNumber
  , KnownNat externalNodesNumber
  ) =>
  Ram dom portsNumber agentType ->
  Signal dom (Interface externalNodesNumber) ->
  Signal dom (Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)))
loadInterface ram interface =
  bundle $
    map
      (traverse readFromRam . sequenceA)
      (unbundle interface)
 where
  partRam address = ram address def
  readFromRam address = case sequenceA (partRam address) of
    Just node -> LoadedNode <$> node <*> address
    Nothing -> error "An attempt to read at a free address"

-- | Load `ActivePair` by `AddressNumber`. It is assumed that `AddressNumber` is actually active
loadActivePair ::
  (KnownDomain dom, HiddenClockResetEnable dom, KnownNat portsNumber) =>
  Ram dom portsNumber agentType ->
  Signal dom AddressNumber ->
  Signal dom (ActivePair portsNumber agentType)
loadActivePair ram leftActiveNodeAddress =
  ActivePair
    <$> (LoadedNode <$> leftActiveNode <*> leftActiveNodeAddress)
    <*> (LoadedNode <$> rightActiveNode <*> rightActiveNodeAddress)
 where
  partRam address = ram address def
  getNodeByAddress address = case sequenceA $ partRam address of
    Just node -> node
    Nothing -> error "An attempt to read at a free address"
  getRightActiveNodeAddress node = case view primaryPort node of
    Connected port -> view nodeAddress port
    NotConnected -> error "Wrong definition of active pair"
  leftActiveNode = getNodeByAddress leftActiveNodeAddress
  rightActiveNodeAddress = getRightActiveNodeAddress <$> leftActiveNode
  rightActiveNode = getNodeByAddress rightActiveNodeAddress

removeActivePairFromRam ::
  Ram dom portsNumber agentType ->
  Signal dom (ActivePair portsNumber agentType) ->
  Vec 2 (Signal dom (Maybe (Node portsNumber agentType)))
removeActivePairFromRam ram acPair = map (\address -> partRam (Just <$> bundle (address, def))) (leftAddress :> rightAddress :> Nil)
 where
  partRam = ram def
  leftAddress = view (leftNode . originalAddress) <$> acPair
  rightAddress = view (rightNode . originalAddress) <$> acPair
