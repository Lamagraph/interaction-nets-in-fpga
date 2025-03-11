module Core.Loader where

import Clash.Prelude
import Control.Lens (view)
import Core.MemoryManager.MemoryManager (ActivePair (ActivePair), leftNode, rightNode)
import Core.MemoryManager.NodeChanges
import Core.Node
import Data.Maybe (fromJust, fromMaybe, isJust)

-- | Type alias for partial applied `blockRam`
type Ram dom portsNumber agentType =
  ( Signal dom AddressNumber ->
    Signal dom (Maybe (AddressNumber, Maybe (Node portsNumber agentType))) ->
    Signal dom (Maybe (Node portsNumber agentType))
  )

-- | Read external `Node`s from ram
loadInterface ::
  ( KnownDomain dom
  , KnownNat portsNumber
  , KnownNat externalNodesNumber
  ) =>
  Ram dom portsNumber agentType ->
  Signal dom (Interface externalNodesNumber) ->
  Signal dom (Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)))
loadInterface ram interface =
  bundle $
    map
      ( \signalMaybeAddress ->
          mux
            (isJust <$> signalMaybeAddress)
            ( let a = fromJust <$> signalMaybeAddress
               in Just <$> readFromRam a
            )
            (pure Nothing)
      )
      (unbundle interface)
 where
  partRam address = ram address def
  readFromRam address = (LoadedNode . fromMaybeErrorX <$> partRam address) <*> address
   where
    fromMaybeErrorX = fromMaybe (error "An attempt to read at a free address")

-- | Load `ActivePair` by `AddressNumber`. It is assumed that `AddressNumber` is actually active
loadActivePair ::
  (KnownDomain dom, KnownNat portsNumber) =>
  Ram dom portsNumber agentType ->
  Signal dom AddressNumber ->
  Signal dom (ActivePair portsNumber agentType)
loadActivePair ram leftActiveNodeAddress =
  ActivePair
    <$> (LoadedNode <$> leftActiveNode <*> leftActiveNodeAddress)
    <*> (LoadedNode <$> rightActiveNode <*> rightActiveNodeAddress)
 where
  partRam address = ram address def
  getNodeByAddress address = fromMaybe (errorX "An attempt to read at a free address") <$> partRam address
  getRightActiveNodeAddress node = view nodeAddress (fromMaybe (errorX "Wrong definition of active pair") $ view primaryPort node)
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

writeChanges ::
  (KnownNat maxNumOfChangedNodes, KnownNat portsNumber, KnownDomain domWrite) =>
  Ram domWrite portsNumber agentType ->
  Signal domWrite (Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber agentType))) ->
  Vec maxNumOfChangedNodes (Signal domWrite (Maybe (Node portsNumber agentType)))
writeChanges ram changes = map writeByLoadedNode (unbundle changes)
 where
  writeByLoadedNode signalMaybeLoadedNode =
    mux
      (isJust <$> signalMaybeLoadedNode)
      (g (fromJust <$> signalMaybeLoadedNode))
      (ram (pure 0) def)
   where
    g signalLoadedNode =
      let f = Just (view originalAddress <$> signalLoadedNode, Just . view containedNode <$> signalLoadedNode)
       in ram (pure 0) (traverse bundle f)
