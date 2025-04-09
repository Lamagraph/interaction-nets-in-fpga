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
  writeNewActives,
  applyChangesToNode,
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
  getChangesByAddress address = fromMaybe (errorX "External node has no changes") (find mapOfChanges address)
  getChangesByLoadedNode loadedNode = getChangesByAddress $ loadedNode ^. originalAddress
  applyChangesToLoadedNode loadedNode change = over containedNode (`applyChangesToNode` change) loadedNode

getActiveAddresses ::
  forall nodesNumber edgesNumber portsNumber agentType.
  (KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber) =>
  Delta nodesNumber edgesNumber portsNumber agentType ->
  Vec (Div nodesNumber 2 + Div edgesNumber 2) (Maybe AddressNumber)
getActiveAddresses (Delta nodes edges _) = newActivesByEdges
 where
  getPrimaryAddressByLoadedNode (LoadedNode (Node (Connected (Port adjacentAddress Primary)) _ _) address) = Just (adjacentAddress, address)
  getPrimaryAddressByLoadedNode _ = Nothing
  newActivesByNodes =
    foldl
      ( \actives maybeLoadedNode ->
          maybe
            actives
            (\(adjacentAddress, address) -> if Just adjacentAddress `notElem` actives then Just address +>> actives else actives)
            (getPrimaryAddressByLoadedNode =<< maybeLoadedNode)
      )
      (def :: Vec (Div nodesNumber 2 + Div edgesNumber 2) (Maybe AddressNumber))
      nodes
  newActivesByEdges =
    foldl
      ( \actives -> \case
          Just (Edge (Connected (Port address Primary)) (Connected (Port _ Primary))) -> Just address +>> actives
          _ -> actives
      )
      newActivesByNodes
      edges

-- | Mark new active `Address` in active bit map
writeNewActives ::
  forall nodesNumber edgesNumber portsNumber cellsNumber agentType.
  (KnownNat nodesNumber, KnownNat edgesNumber, KnownNat cellsNumber, KnownNat portsNumber) =>
  Delta nodesNumber edgesNumber portsNumber agentType ->
  MemoryManager cellsNumber ->
  MemoryManager cellsNumber
writeNewActives delta memoryManager = set activePairs newActivePairs memoryManager
 where
  newActivePairs =
    foldl
      (\acPairs maybeAddress -> maybe acPairs (markAddress acPairs True) maybeAddress)
      (memoryManager ^. activePairs)
      (getActiveAddresses delta)
