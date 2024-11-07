{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.MemoryManager where

import Clash.Prelude
import Control.Lens hiding (ifoldl)
import Core.Map
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

data MemoryManager (cellsNumber :: Nat) -- (portsNumber :: Nat)
  = MemoryManager
  { _busyBitMap :: Vec cellsNumber Bool -- map Address : Bool. tell smth like "this Address is busy, so you can not to write here"
  , _activePairs :: Vec cellsNumber Bool
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
  Vec cellsNumber Bool ->
  ActivePair portsNumber ->
  Vec cellsNumber Bool
deleteActivePair oldActivePairs activePairToDelete =
  if leftInVec `xor` rightInVec
    then newActivePairs
    else error "In active pairs map should be exact one address"
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
  (Vec cellsNumber Bool, LoadedNode portsNumber)
updateFromLocalToLoaded busyMap localNode = (newBusyMap, LoadedNode node newAddress)
 where
  newAddress = registerAddressNumToNewNode busyMap
  newBusyMap = markAddress busyMap True newAddress
  node = localNode ^. numberedNode

-- | Data to accumulate all `Port` changes of the `Node`
data NodePortsInfo (portsNumber :: Nat)
  = NodePortInfo
  { _secP :: Vec portsNumber (Maybe (Port portsNumber))
  , _primeP :: Maybe (Port portsNumber)
  }
  deriving (Show, Eq, Generic, NFDataX, Default)

$(makeLenses ''NodePortsInfo)

-- | Update `Port`s of `Node` by `NodePortsInfo`
updateNode ::
  (KnownNat portsNumber) =>
  Node portsNumber ->
  NodePortsInfo portsNumber ->
  Node portsNumber
updateNode oldNode (NodePortInfo maybeSecPortsAddr maybePrimaryPort) =
  set primaryPort newPrimPort (set secondaryPorts newSecPorts oldNode)
 where
  newSecPorts =
    zipWith
      (<|>)
      maybeSecPortsAddr
      (oldNode ^. secondaryPorts)
  newPrimPort = case maybePrimaryPort of
    Nothing -> oldNode ^. primaryPort
    Just primPort -> primPort

-- | The flag to distinguish between nodes that need to be loaded from ram and local ones
data LocalFlag (portsNumber :: Nat)
  = Local
  | External
  deriving (Eq)

{- | Update ports info in attached `Node`.
It accumulates in key-value map, where key is `ActualAddressNum` and value is a pair of `NodePortsInfo` and `LocalFlag` to distinguish local and external nodes.
The first one we can upload from locals and another one we have to upload from ram
-}
updatePortsInfoByPort ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag portsNumber) ->
  LoadedNode portsNumber ->
  Port portsNumber ->
  IdOfPort portsNumber ->
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag portsNumber)
updatePortsInfoByPort infoVec localNodeWithAddress (Port maybeAddr connectedToPortId) portId =
  case maybeAddr of
    Nothing -> infoVec -- or @error "Port must be connected"@
    Just addr -> case addr of
      LocalAddress localAddressNum -> update Local localAddressNum
      ActualAddress addressNum -> update External addressNum
     where
      update localFlag addressNum =
        changeValueByKey
          infoVec
          ( \case
              Nothing -> Just (constructNewInfo def, localFlag)
              Just (info, _) -> Just (constructNewInfo info, localFlag)
          )
          addressNum
       where
        newPort = Port (Just $ ActualAddress (localNodeWithAddress ^. originalAddress)) portId
        constructNewInfo info@(NodePortInfo secPortsInfo _) = case connectedToPortId of
          Primary -> set primeP (Just newPort) info
          Id i -> set secP (replace i (Just newPort) secPortsInfo) info

updatePortsInfoByEdge ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag portsNumber) ->
  Edge portsNumber ->
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag portsNumber)
updatePortsInfoByEdge oldInfoVec (Edge leftE rightE) =
  update
    rightAddrNum
    leftAddrNum
    (rightE ^. idOfPort)
    (leftE ^. idOfPort)
    $ update leftAddrNum rightAddrNum (leftE ^. idOfPort) (rightE ^. idOfPort) oldInfoVec
 where
  leftAddrNum = leftE ^. addressOfVertex
  rightAddrNum = rightE ^. addressOfVertex
  update addrToUpdate addrToWhichUpdate connectedToPortId portId infoVec =
    changeValueByKey
      infoVec
      ( \case
          Nothing -> Just (constructNewInfo (NodePortInfo def Nothing), External)
          Just (info, _) -> Just (constructNewInfo info, External)
      )
      addrToUpdate
   where
    newPort = Port (Just $ ActualAddress addrToWhichUpdate) portId
    constructNewInfo info@(NodePortInfo secPortsInfo _) = case connectedToPortId of
      Primary -> set primeP (Just newPort) info
      Id i -> set secP (replace i (Just newPort) secPortsInfo) info

-- Check if `Node` is active
newNodeIsActive ::
  (KnownNat portsNumber) => Node portsNumber -> Bool
newNodeIsActive node =
  case node ^. primaryPort . portConnectedToId of
    Primary -> True
    _ -> False

updateMM ::
  forall (cellsNumber :: Nat) (portsNumber :: Nat) (edgeNumber :: Nat) (dom :: Domain) (maxNumOfChangedNodes :: Nat).
  ( KnownNat cellsNumber
  , KnownNat portsNumber
  , KnownNat edgeNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber ~ 16
  , 1 <= edgeNumber
  , CLog 2 edgeNumber ~ 16
  , KnownNat maxNumOfChangedNodes
  ) =>
  ( Signal dom ActualAddressNumber ->
    Signal dom (Maybe (ActualAddressNumber, Maybe (Node portsNumber))) ->
    Signal dom (Maybe (Node portsNumber))
  ) ->
  Signal dom (MemoryManager cellsNumber) ->
  Signal dom (Delta cellsNumber edgeNumber portsNumber) ->
  ( Signal dom (MemoryManager cellsNumber)
  , Signal dom ActualAddressNumber ->
    Signal dom (Maybe (ActualAddressNumber, Maybe (Node portsNumber))) ->
    Signal dom (Maybe (Node portsNumber))
  )
-- it is possible to merge all foldl into one
updateMM ram memoryManager delta = (MemoryManager <$> markedBusyBitMap <*> newActives, ram)
 where
  localNodesSignal = unbundle $ (^. newNodes) <$> delta :: _ (Signal _ _)
  activePairSignal = (^. activePair) <$> delta
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
                Just pair -> sequenceA $ Just $ (^. originalAddress) <$> pair
              newLocalToActual = case sequenceA actualAddr of
                Nothing -> localToActual
                Just addr -> case sequenceA signalMaybeLocalNode of
                  Nothing -> localToActual
                  Just signalLocal -> updateLocalActual <$> localToActual <*> signalLocal <*> addr
           in (newBusyMap, (+>>) <$> newLoadedNode <*> loadedNodes, newLocalToActual)
      )
      (freedFromActivePair, newLoadedNodesStart, localActualMapDef)
      localNodesSignal
   where
    newLoadedNodesStart = pure def :: Signal dom (Vec cellsNumber (Maybe _))
    signalMaybeApply signalMaybeLocalNode busyMap = case sequenceA signalMaybeLocalNode of
      Nothing -> (busyMap, pure Nothing)
      Just signalLocalNode ->
        let (signalBusyMap, loadedSignal) = unbundle (updateFromLocalToLoaded <$> busyMap <*> signalLocalNode)
         in (signalBusyMap, Just <$> loadedSignal)
    updateLocalActual oldMap localNode actualAddr x =
      if x == (localNode ^. localAddress)
        then LoadedNode (localNode ^. numberedNode) actualAddr
        else oldMap (localNode ^. localAddress)
  -- accumulate all port changes from local nodes
  infoAboutUpdatesByNodes =
    foldl
      ( \infoVec signalMaybeLoadedNode -> case sequenceA signalMaybeLoadedNode of
          Nothing -> infoVec
          Just signalLoadedNode ->
            let infoVecByPrimary =
                  updatePortsInfoByPort
                    <$> infoVec
                    <*> signalLoadedNode
                    <*> ((^. containedNode . primaryPort) <$> signalLoadedNode)
                    <*> pure Primary
             in ifoldl
                  ( \oldInfoVec i signalMaybePort ->
                      case sequenceA signalMaybePort of
                        Nothing -> oldInfoVec
                        Just signalPort ->
                          updatePortsInfoByPort
                            <$> oldInfoVec
                            <*> signalLoadedNode
                            <*> signalPort
                            <*> pure (Id i)
                  )
                  infoVecByPrimary
                  (unbundle ((^. containedNode . secondaryPorts) <$> signalLoadedNode))
      )
      (def :: Signal dom (Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag portsNumber)))
      (unbundle loaded)
  -- accumulate all ports changes from edges. i.e. connect some external nodes with each other
  infoAboutAllUpdates =
    foldl
      ( \infoVec signalMaybeEdge -> case sequenceA signalMaybeEdge of
          Nothing -> infoVec
          Just signalEdge -> updatePortsInfoByEdge <$> infoVec <*> signalEdge
      )
      infoAboutUpdatesByNodes
      (unbundle $ (^. newEdges) <$> delta)
  -- read all necessary external nodes from ram and update them with the local ones
  nodesToWrite =
    map
      ( \signalMaybePair ->
          case sequenceA signalMaybePair of
            Nothing -> pure Nothing :: Signal dom _
            Just signalPair ->
              let addr = fst <$> signalPair
                  signalMaybeInfo = snd <$> signalPair
               in case sequenceA signalMaybeInfo of
                    Nothing -> def -- same as @pure Nothing@
                    Just signalInfo ->
                      let lf = snd <$> signalInfo
                          info = fst <$> signalInfo
                          node = readByAddressAndFlag addr lf
                       in mux
                            (lf .==. pure External)
                            (sequenceA $ Just $ LoadedNode <$> (updateNode <$> node <*> info) <*> addr)
                            ( sequenceA $
                                Just $
                                  LoadedNode <$> (updateNode <$> node <*> info) <*> ((^. originalAddress) <$> (localActualMap <*> addr))
                            )
      )
      (unbundle infoAboutAllUpdates)
  -- write all changes into the ram
  _ =
    map
      (maybe def writeByLoadedNode . sequenceA)
      nodesToWrite
  -- update active pairs (maybe it should to replace into the first foldl)
  newActives =
    foldl
      ( \actives signalMaybeLocalNode -> case sequenceA signalMaybeLocalNode of
          Nothing -> actives
          Just signalLocalNode ->
            let addressIsActive = (newNodeIsActive . (^. containedNode) <$> signalLocalNode)
             in mux addressIsActive ((replace . (^. originalAddress) <$> signalLocalNode) <*> addressIsActive <*> actives) actives
      )
      pairsAfterDelete
      (unbundle loaded)

  readByAddressAndFlag addressNumber localFlag = mux (localFlag .==. pure External) externalCase localCase
   where
    externalCase = case sequenceA $ ram addressNumber (pure Nothing) of
      Nothing -> error "There is no Node by this address"
      Just node -> node
    localCase = (^. containedNode) <$> (localActualMap <*> addressNumber)
  writeByLoadedNode loadedNode =
    ram
      ((^. originalAddress) <$> loadedNode)
      (sequenceA $ Just $ bundle ((^. originalAddress) <$> loadedNode, sequenceA $ Just $ (^. containedNode) <$> loadedNode))
