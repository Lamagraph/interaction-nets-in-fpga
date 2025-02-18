module Core.MemoryManager.NodeChanges (
  Changes (..),
  secP,
  primeP,
  Edge (..),
  leftEnd,
  rightEnd,
  Delta (..),
  newNodes,
  newEdges,
  activePair,
  Interface,
  updateLoadedNodesByChanges,
) where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, imap, (:>))
import Core.Map
import Core.MemoryManager.MemoryManager
import Core.Node
import Data.Maybe

data Delta (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) agentType = Delta
  { _newNodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  , _newEdges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _activePair :: ActivePair portsNumber agentType
  }
  deriving (Show, Eq, Generic, NFDataX)

$(makeLenses ''Delta)

type Interface externalNodesNumber = Vec externalNodesNumber (Maybe AddressNumber)

-- | Data to accumulate all `Port` changes of the `Node`
data Changes (portsNumber :: Nat)
  = Changes
  { _secP :: Vec portsNumber (Maybe (Connection portsNumber))
  , _primeP :: Maybe (Connection portsNumber)
  }
  deriving (Show, Eq, Generic, NFDataX, Default)

$(makeLenses ''Changes)

-- | Update `Port`s of `Node` by `Changes`
applyChangesToNode ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Changes portsNumber ->
  Node portsNumber agentType
applyChangesToNode oldNode (Changes maybeSecPortsAddr maybePrimaryPort) =
  set primaryPort newPrimaryPort (set secondaryPorts newSecondaryPorts oldNode)
 where
  newSecondaryPorts =
    zipWith
      (<|>)
      maybeSecPortsAddr
      (oldNode ^. secondaryPorts)
  newPrimaryPort = case maybePrimaryPort of
    Nothing -> oldNode ^. primaryPort
    Just primPort -> primPort

-- | Update external `LoadedNode`s by accumulated `Changes`
updateLoadedNodesByChanges ::
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber agentType)) ->
  Map maxNumOfChangedNodes (Changes portsNumber) ->
  Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber agentType))
updateLoadedNodesByChanges externalLoadedNodes mapOfChanges =
  map
    (\maybeLoadedNode -> applyChangesToLoadedNode <$> maybeLoadedNode <*> (getChangesByLoadedNode <$> maybeLoadedNode))
    externalLoadedNodes
 where
  getChangesByAddress address = fromMaybe (error "some error") (find mapOfChanges address) -- TODO: think about error message
  getChangesByLoadedNode loadedNode = getChangesByAddress $ loadedNode ^. originalAddress
  applyChangesToLoadedNode loadedNode change = over containedNode (`applyChangesToNode` change) loadedNode
