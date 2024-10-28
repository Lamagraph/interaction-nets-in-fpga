{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core.MemoryManager where

import Clash.Prelude
import Control.Lens hiding (ifoldl)
import Core.Node

data EdgeEnd (portsNumber :: Nat) = EdgeEnd
  { _addressOfVertex :: AddressNumber
  , _idOfPort :: IdOfPort portsNumber
  }
  deriving (Generic, NFDataX, Show, Eq)

$(makeLenses ''EdgeEnd)

data Edge (portsNumber :: Nat) = Edge
  { _leftEnd :: EdgeEnd portsNumber
  , _rightEnd :: EdgeEnd portsNumber
  }
  deriving (Generic, NFDataX, Show, Eq)

$(makeLenses ''Edge)
data ActivePair (portsNumber :: Nat) = ActivePair
  { _leftNode :: LoadedNode portsNumber
  , _rightNode :: LoadedNode portsNumber
  }
  deriving (Show, Eq, Generic, NFDataX, Bundle)
$(makeLenses ''ActivePair)

data MemoryManager (cellsNumber :: Nat) (portsNumber :: Nat) = MemoryManager
  { _busyBitMap :: Vec cellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _activePairs :: Vec cellsNumber (Maybe (ActivePair portsNumber))
  , _ram :: ActualAddressNumber -> Node portsNumber
  }
  deriving (Generic, NFDataX, Bundle)
$(makeLenses ''MemoryManager)

data Delta (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = Delta
  { _newNodes :: Vec nodesNumber (Maybe (LocalNode portsNumber))
  , _newEdges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _activePair :: ActivePair portsNumber
  }
  deriving (Show, Eq, Generic, NFDataX)
$(makeLenses ''Delta)

-- | Get address from memory manager that is not busy. Return `Nothing` if all addresses are busy
getUnusedAddress ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber ~ 16) =>
  Vec cellsNumber Bool ->
  Maybe ActualAddressNumber
getUnusedAddress busyMap = address
 where
  indexOfUnused = elemIndex False busyMap
  address = bitCoerce <$> indexOfUnused

-- | Mark given `AddressNumber` as busy or not according to passed flag (`True` means busy)
markAddress ::
  (KnownNat cellsNumber) =>
  Vec cellsNumber Bool ->
  Bool ->
  ActualAddressNumber ->
  Vec cellsNumber Bool
markAddress busyMap marker address =
  replace address marker busyMap

-- | Replace processed active pair at `Nothing` in `Vec` of active pairs
deleteActivePair ::
  (KnownNat cellsNumber, KnownNat portsNumber) =>
  Vec cellsNumber (Maybe (ActivePair portsNumber)) ->
  ActivePair portsNumber ->
  Vec cellsNumber (Maybe (ActivePair portsNumber))
deleteActivePair nowActivePairs activePairToDelete = apReplaced
 where
  apReplaced = case elemIndex (Just activePairToDelete) nowActivePairs of
    Nothing -> error ""
    Just i -> replace i Nothing nowActivePairs

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

-- | Give unused `AddressNumber` to `LocalNode`
registerAddressNumToNewNode ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber ~ 16) =>
  Vec cellsNumber Bool ->
  ActualAddressNumber
registerAddressNumToNewNode busyMap = addressNum
 where
  addressNum = case getUnusedAddress busyMap of
    Nothing -> error "Memory space is over"
    Just address -> address

{- | Assign actual `AddressNumber` to `LocalNode` and mark busy bit map according to this.
It is the composition of `registerAddressNumToNewNode` and `markAddress` just for usability
-}
updateFromLocalToLoaded ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber ~ 16, KnownNat portsNumber) =>
  Vec cellsNumber Bool ->
  LocalNode portsNumber ->
  (Vec cellsNumber Bool, (ActualAddressNumber, Node portsNumber))
updateFromLocalToLoaded busyMap localNode = (newBusyMap, (newAddress, node))
 where
  newAddress = registerAddressNumToNewNode busyMap
  newBusyMap = markAddress busyMap True newAddress
  node = localNode ^. numberedNode

-- | Update ram function by `Node`. So at given `AddressNumber` it return given `Node`, otherwise there are no changes
updateLoaderByNode ::
  (KnownNat portsNumber) =>
  (ActualAddressNumber -> Node portsNumber) ->
  AddressNumber ->
  Node portsNumber ->
  (ActualAddressNumber -> Node portsNumber)
updateLoaderByNode oldRam newAddress node = newRam
 where
  newRam address = if address == newAddress then node else oldRam address

-- | The same as `updateLoaderByNode` but with `LoadedNode` instead of (`AddressNumber` and `Node`)
updateLoaderByLoadedNode ::
  (KnownNat portsNumber) =>
  (ActualAddressNumber -> Node portsNumber) ->
  LoadedNode portsNumber ->
  (ActualAddressNumber -> Node portsNumber)
updateLoaderByLoadedNode oldRam loadedNode = updateLoaderByNode oldRam (loadedNode ^. originalAddress) (loadedNode ^. containedNode)

-- | Add `ActivePair` if it appeared after reduction by given `AddressNumber`
updateActivesByGivenAddress ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber ~ 16, KnownNat portsNumber) =>
  Vec cellsNumber (Maybe (ActivePair portsNumber)) ->
  (AddressNumber -> Node portsNumber) ->
  AddressNumber ->
  Vec cellsNumber (Maybe (ActivePair portsNumber))
updateActivesByGivenAddress pairs ramMM addressNumberOfInternalNode =
  case internalNode ^. primaryPort . nodeAddress of
    Nothing -> error "Port must be connected" -- for now, we need to rewrite it in the future
    Just a -> case a of
      LocalAddress _ -> error "You can not to update active pairs before load all local nodes"
      ActualAddress addrNum -> if isActive lln rln then Just (ActivePair lln rln) +>> pairs else pairs
       where
        lln = LoadedNode (ramMM addrNum) addrNum
        rln = LoadedNode internalNode addressNumberOfInternalNode
 where
  internalNode = ramMM addressNumberOfInternalNode

-- | Set concrete `Port` in concrete `Node` according to its type (primary or secondary)
updateOnePort ::
  (KnownNat portsNumber) =>
  Node portsNumber ->
  Port portsNumber ->
  Node portsNumber
updateOnePort node port = case port ^. portConnectedToId of
  Primary -> set primaryPort port node
  Id portId -> set secondaryPorts (replace portId (Just port) (node ^. secondaryPorts)) node

{- | Update `Port` in `Node` that connected by given `Port`.
It may seems strange, that we update not given port, but connected to this.
But it allowed to unify the update of external and internal nodes
-}
updateConnectedToPort ::
  (KnownNat portsNumber) =>
  (LocalAddressNumber -> ActualAddressNumber) ->
  (ActualAddressNumber -> Node portsNumber) ->
  ActualAddressNumber ->
  Port portsNumber ->
  LoadedNode portsNumber
updateConnectedToPort localToActual loader nodeAddr port = LoadedNode newConnectedNode addr
 where
  (oldConnectedNode, addr) = case port ^. nodeAddress of
    Nothing -> error "Port must to be connected"
    Just a -> case a of
      ActualAddress a' -> (loader a', a')
      LocalAddress a' -> (loader $ localToActual a', a')
  newPort = set nodeAddress (Just $ ActualAddress nodeAddr) port
  newConnectedNode = updateOnePort oldConnectedNode newPort

-- | Update external `LoadedNode`s by `Edge`, i.e. actually connect the nodes on the ports that were connected to the disappeared nodes
updateExternalPortByEdge ::
  (KnownNat portsNumber) =>
  (ActualAddressNumber -> Node portsNumber) ->
  Edge portsNumber ->
  (LoadedNode portsNumber, LoadedNode portsNumber)
updateExternalPortByEdge oldRam edge = (LoadedNode leftNewNode leftExtNodeAddress, LoadedNode rightNewNode rightExtNodeAddress)
 where
  leftExtNodeAddress = edge ^. leftEnd . addressOfVertex
  rightExtNodeAddress = edge ^. rightEnd . addressOfVertex
  leftNewPort = Port (Just $ ActualAddress rightExtNodeAddress) (edge ^. rightEnd . idOfPort)
  rightNewPort = Port (Just $ ActualAddress leftExtNodeAddress) (edge ^. leftEnd . idOfPort)
  leftNewNode = updateOnePort (oldRam leftExtNodeAddress) leftNewPort
  rightNewNode = updateOnePort (oldRam rightExtNodeAddress) rightNewPort

updateMM ::
  forall (cellsNumber :: Nat) (portsNumber :: Nat) (edgeNumber :: Nat) (dom :: Domain).
  ( KnownNat cellsNumber
  , KnownNat portsNumber
  , KnownNat edgeNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber ~ 16
  , 1 <= edgeNumber
  , CLog 2 edgeNumber ~ 16
  ) =>
  Signal dom (MemoryManager cellsNumber portsNumber) ->
  Signal dom (Delta cellsNumber edgeNumber portsNumber) ->
  Signal dom (MemoryManager cellsNumber portsNumber)
updateMM memoryManager delta = MemoryManager <$> markedBusyBitMap <*> newActives <*> updatedByEdges
 where
  localNodesSignal = unbundle $ (^. newNodes) <$> delta :: _ (Signal _ _)
  activePairSignal = (^. activePair) <$> delta
  ramSignal = (^. ram) <$> memoryManager
  -- map of local and actual `AddressNumber`
  localActualMapDef = undefined
  -- removed the processed active pair
  pairsAfterDelete = (deleteActivePair . (^. activePairs) <$> memoryManager) <*> activePairSignal
  -- freed up space from an active pair
  freedFromActivePair = (freeUpActivePair . (^. busyBitMap) <$> memoryManager) <*> activePairSignal
  -- gave the local nodes free addresses, marking the occupied ones. Received a marked map address:busy, a list of new nodes with their actual addresses and a map (local address):(actual address)
  (markedBusyBitMap, loaded, localActualMap) =
    foldl
      ( \(busyMap, loadedNodes, localToActual) signalMaybeLocalNode ->
          let (newBusyMap, newLoadedNode) = signalMaybeApply signalMaybeLocalNode busyMap
              actualAddr = case sequenceA newLoadedNode of
                Nothing -> pure Nothing
                Just pair -> sequenceA $ Just $ fst <$> pair
              newLocalToActual = case sequenceA actualAddr of
                Nothing -> localToActual
                Just addr -> case sequenceA signalMaybeLocalNode of
                  Nothing -> localToActual
                  Just signalLocal -> updateLocalActual <$> localToActual <*> ((^. localAddress) <$> signalLocal) <*> addr
           in (newBusyMap, (+>>) <$> newLoadedNode <*> loadedNodes, newLocalToActual)
      )
      (freedFromActivePair, newLoadedNodesStart, localActualMapDef)
      localNodesSignal
   where
    newLoadedNodesStart = pure def :: Signal _ (Vec cellsNumber (Maybe _))
    signalMaybeApply signalMaybeLocalNode busyMap = case sequenceA signalMaybeLocalNode of
      Nothing -> (busyMap, pure Nothing)
      Just signalLocalNode ->
        let (signalBusyMap, loadedSignal) = unbundle (updateFromLocalToLoaded <$> busyMap <*> signalLocalNode)
         in (signalBusyMap, Just <$> loadedSignal)
  -- updated the ram according to internal changes, There are still ports that refer to the local address
  updatedByLocalsRam =
    foldl
      ( \loader signalMaybeLoaded ->
          case sequenceA signalMaybeLoaded of
            Nothing -> loader
            Just signalLoaded ->
              let (addrS, nodeS) = unbundle signalLoaded
               in updateLoaderByNode <$> loader <*> addrS <*> nodeS
      )
      ramSignal
      (unbundle loaded)
  -- updated the ram so that the ports refer to the actual (non-local) address. The connections of the external nodes have also been updated
  updatedByNodesConnection =
    foldl
      ( \loader addrNode -> case sequenceA addrNode of
          Nothing -> loader
          Just x ->
            let (addr, node) = unbundle x
             in foldl
                  ( \loader' mbPort -> case sequenceA mbPort of
                      Nothing -> loader'
                      Just port -> updateLoaderByLoadedNode <$> loader' <*> (updateConnectedToPort <$> localActualMap <*> loader' <*> addr <*> port)
                  )
                  loader
                  (unbundle ((^. secondaryPorts) <$> node))
      )
      updatedByLocalsRam
      (unbundle loaded)
  -- updated the ram so that the external nodes, which became actually connected to each other through an edge after reduction, referred to each other by ports
  updatedByEdges =
    foldl
      ( \loader signalMaybeEdge -> case sequenceA signalMaybeEdge of
          Nothing -> loader
          Just signalEdge ->
            let (leftLN, rightLN) = unbundle $ updateExternalPortByEdge <$> loader <*> signalEdge
             in updateLoaderByLoadedNode <$> (updateLoaderByLoadedNode <$> loader <*> leftLN) <*> rightLN
      )
      updatedByNodesConnection
      (unbundle ((^. newEdges) <$> delta))
  -- updated the active pairs
  newActives =
    foldl
      ( \actives signalMaybeLoaded ->
          case sequenceA signalMaybeLoaded of
            Nothing -> actives
            Just signalLoaded ->
              let (addrS, _) = unbundle signalLoaded
               in updateActivesByGivenAddress <$> actives <*> updatedByEdges <*> addrS
      )
      pairsAfterDelete
      (unbundle loaded)
  updateLocalActual oldMap localAddr actualAddr x = if x == localAddr then actualAddr else oldMap localAddr
