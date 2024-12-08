module Core.MemoryManager.NodeChanges (
  Changes (..),
  secP,
  primeP,
  updateLoadedNodesByChanges,
) where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, imap, (:>))
import Core.Map
import Core.Node
import Data.Maybe

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
  Node portsNumber ->
  Changes portsNumber ->
  Node portsNumber
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
  Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber)) ->
  Map maxNumOfChangedNodes (Changes portsNumber) ->
  Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber))
updateLoadedNodesByChanges externalLoadedNodes mapOfChanges =
  map
    (\maybeLoadedNode -> applyChangesToLoadedNode <$> maybeLoadedNode <*> (getChangesByLoadedNode <$> maybeLoadedNode))
    externalLoadedNodes
 where
  getChangesByAddress address = fromMaybe (error "some error") (find mapOfChanges address) -- TODO: think about error message
  getChangesByLoadedNode loadedNode = getChangesByAddress $ loadedNode ^. originalAddress
  applyChangesToLoadedNode loadedNode change = over containedNode (`applyChangesToNode` change) loadedNode
