{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.MemoryManager where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, imap, (:>))
import Core.Map
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

data MemoryManager (cellsNumber :: Nat)
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

{- | Get address from memory manager that is not busy. Return `Nothing` if all addresses are busy

==== __Example__

>>> getUnusedAddress (repeat False :: Vec 65536 Bool)
Just 0
-}
getUnusedAddress ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber) =>
  Vec cellsNumber Bool ->
  Maybe ActualAddressNumber
getUnusedAddress busyMap = address
 where
  indexOfUnused = elemIndex False busyMap
  address = indexToUnsigned <$> indexOfUnused

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
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber) =>
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
getLoadedFromLocal ::
  (KnownNat cellsNumber, 1 <= cellsNumber, CLog 2 cellsNumber <= BitSize AddressNumber, KnownNat portsNumber) =>
  Vec cellsNumber Bool ->
  LocalNode portsNumber ->
  (Vec cellsNumber Bool, LoadedNode portsNumber)
getLoadedFromLocal busyMap localNode = (newBusyMap, LoadedNode node newAddress)
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
updateNodeByPortsInfo ::
  (KnownNat portsNumber) =>
  Node portsNumber ->
  NodePortsInfo portsNumber ->
  Node portsNumber
updateNodeByPortsInfo oldNode (NodePortInfo maybeSecPortsAddr maybePrimaryPort) =
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
data LocalFlag
  = Local
  | External
  deriving (Eq)

{- | Update ports info in attached `Node`.
It accumulates in key-value map, where key is `ActualAddressNumber` and value is a pair of `NodePortsInfo` and `LocalFlag` to distinguish local and external nodes.
The first one we can load from locals and another one we have to load from ram
-}
updatePortsInfoByPort ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag) ->
  LoadedNode portsNumber ->
  Port portsNumber ->
  IdOfPort portsNumber ->
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag)
updatePortsInfoByPort infoVec localNodeWithAddress (Port maybeAddr connectedToPortId) portId =
  case maybeAddr of
    Nothing -> infoVec -- or @error "Port must be connected"@
    Just addr -> case addr of
      LocalAddress localAddressNum -> update Local localAddressNum
      ActualAddress addressNum -> update External addressNum
     where
      update localFlag addressNum =
        insertWith
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

{- | The same as `updatePortsInfoByPort`, but updating happens by `Edge`.
In fact it accumulate info about which external `LoadedNode` have become connected with each other
-}
updatePortsInfoByEdge ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag) ->
  Edge portsNumber ->
  Map maxNumOfChangedNodes (NodePortsInfo portsNumber, LocalFlag)
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
    insertWith
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

-- | Assign new `ActualAddressNumber` to `LocalNode` and mark busy bit map
assignNewAddressToLocalNode ::
  (CLog 2 cellsNumber <= BitSize AddressNumber, KnownNat cellsNumber, KnownNat portsNumber, 1 <= cellsNumber) =>
  Signal dom (Maybe (LocalNode portsNumber)) ->
  Signal dom (Vec cellsNumber Bool) ->
  (Signal dom (Vec cellsNumber Bool), Signal dom (Maybe (LoadedNode portsNumber)))
assignNewAddressToLocalNode signalMaybeLocalNode busyMap = case sequenceA signalMaybeLocalNode of
  Nothing -> (busyMap, def)
  Just signalLocalNode ->
    let (signalBusyMap, loadedSignal) = unbundle (getLoadedFromLocal <$> busyMap <*> signalLocalNode)
     in (signalBusyMap, Just <$> loadedSignal)

-- | Update local-to-loaded map
updateLocalAddressToLoaded ::
  (KnownDomain dom, KnownNat portsNumber) =>
  (Signal dom LocalAddressNumber -> Signal dom (LoadedNode portsNumber)) ->
  Signal dom (LocalNode portsNumber) ->
  Signal dom ActualAddressNumber ->
  Signal dom LocalAddressNumber ->
  Signal dom (LoadedNode portsNumber)
updateLocalAddressToLoaded oldMap localNode actualAddr x =
  mux
    (x .==. (view localAddress <$> localNode))
    ((LoadedNode . view numberedNode <$> localNode) <*> actualAddr)
    (oldMap (view localAddress <$> localNode))

-- | Accumulate all changes by ports of the given `Node` and write it in the `Map`
accumulatePortsChangesByLoadedNode ::
  (KnownNat portsNumber1, KnownNat maxNumOfChangedNodes) =>
  Signal dom (Map maxNumOfChangedNodes (NodePortsInfo portsNumber1, LocalFlag)) ->
  Signal dom (LoadedNode portsNumber1) ->
  Signal dom (Map maxNumOfChangedNodes (NodePortsInfo portsNumber1, LocalFlag))
accumulatePortsChangesByLoadedNode infoVec signalLoadedNode =
  let infoVecByPrimary =
        updatePortsInfoByPort
          <$> infoVec
          <*> signalLoadedNode
          <*> (view (containedNode . primaryPort) <$> signalLoadedNode)
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
        (unbundle (view (containedNode . secondaryPorts) <$> signalLoadedNode))

-- | Type alias for partial applied `blockRam`
type Ram dom portsNumber =
  ( Signal dom ActualAddressNumber ->
    Signal dom (Maybe (ActualAddressNumber, Maybe (Node portsNumber))) ->
    Signal dom (Maybe (Node portsNumber))
  )

updateMM ::
  forall (cellsNumber :: Nat) (portsNumber :: Nat) (edgeNumber :: Nat) (dom :: Domain) (maxNumOfChangedNodes :: Nat).
  ( KnownNat cellsNumber
  , KnownNat portsNumber
  , KnownNat edgeNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , 1 <= edgeNumber
  , CLog 2 edgeNumber <= BitSize AddressNumber
  , KnownNat maxNumOfChangedNodes
  , KnownDomain dom
  ) =>
  Ram dom portsNumber ->
  Signal dom (MemoryManager cellsNumber) ->
  Signal dom (Delta cellsNumber edgeNumber portsNumber) ->
  ( Signal dom (MemoryManager cellsNumber)
  , Ram dom portsNumber
  )
-- it is possible to merge all foldl into one
updateMM ram memoryManager delta = (MemoryManager <$> markedBusyBitMap <*> newActives, ram)
 where
  localNodesSignal = unbundle $ view newNodes <$> delta :: _ (Signal _ _)
  activePairSignal = view activePair <$> delta
  -- map of local and actual `AddressNumber`
  localActualMapDef = error "Nothing is written at this local address"
  updatesInfo = def :: Signal dom (Map maxNumOfChangedNodes (NodePortsInfo _, LocalFlag))
  -- removed the processed active pair
  pairsAfterDelete = (deleteActivePair . view activePairs <$> memoryManager) <*> activePairSignal
  -- freed up space from an active pair
  freedFromActivePair = (freeUpActivePair . view busyBitMap <$> memoryManager) <*> activePairSignal
  {- gave the local nodes free addresses, marking the occupied ones.
     Received a marked map address:busy, a list of new nodes with their actual addresses and a map (local address):(loaded node).
     It also accumulate all changes in ports in the `Map` and updates actives-}
  (markedBusyBitMap, localAddressToLoadedMap, newActives, infoAboutUpdatesByNodes) =
    foldl
      ( \(busyMap, localToActual, oldActives, accumulatedInfoMap) signalMaybeLocalNode ->
          let (newBusyMap, newLoadedNode) = assignNewAddressToLocalNode signalMaybeLocalNode busyMap
              (actualAddr, actives, infoVec) = case sequenceA newLoadedNode of
                Nothing -> (def, oldActives, accumulatedInfoMap)
                Just localLoadedNode ->
                  let addressIsActive = (newNodeIsActive . view containedNode <$> localLoadedNode)
                   in ( sequenceA $ Just $ view originalAddress <$> localLoadedNode
                      , mux addressIsActive ((replace . view originalAddress <$> localLoadedNode) <*> addressIsActive <*> actives) actives
                      , accumulatePortsChangesByLoadedNode accumulatedInfoMap localLoadedNode
                      )
              newLocalToActual = case sequenceA actualAddr of
                Nothing -> localToActual
                Just addr -> case sequenceA signalMaybeLocalNode of
                  Nothing -> localToActual
                  Just signalLocal -> updateLocalAddressToLoaded localToActual signalLocal addr
           in (newBusyMap, newLocalToActual, actives, infoVec)
      )
      ( freedFromActivePair
      , localActualMapDef
      , pairsAfterDelete
      , updatesInfo
      )
      localNodesSignal
  -- accumulate all ports changes from edges. i.e. connect some external nodes with each other
  infoAboutAllUpdates =
    foldl
      ( \infoVec signalMaybeEdge -> case sequenceA signalMaybeEdge of
          Nothing -> infoVec
          Just signalEdge -> updatePortsInfoByEdge <$> infoVec <*> signalEdge
      )
      infoAboutUpdatesByNodes
      (unbundle $ view newEdges <$> delta)
  -- read all necessary external nodes from ram and update them with the local ones
  nodesToWrite =
    map
      ( \signalMaybePair ->
          case sequenceA signalMaybePair of
            Nothing -> def -- same as @pure Nothing@
            Just signalPair ->
              let addr = fst <$> signalPair :: Signal dom AddressNumber
                  signalMaybeInfo = snd <$> signalPair
               in case sequenceA signalMaybeInfo of
                    Nothing -> def
                    Just signalInfo ->
                      let localFlag = snd <$> signalInfo
                          info = fst <$> signalInfo
                          node = readByAddressAndFlag addr localFlag
                       in mux
                            (localFlag .==. pure External)
                            (sequenceA $ Just $ LoadedNode <$> (updateNodeByPortsInfo <$> node <*> info) <*> addr)
                            ( sequenceA $
                                Just $
                                  LoadedNode <$> (updateNodeByPortsInfo <$> node <*> info) <*> (view originalAddress <$> localAddressToLoadedMap addr)
                            )
      )
      (unbundle infoAboutAllUpdates)
  -- write all changes into the ram
  _ =
    map
      (maybe def writeByLoadedNode . sequenceA)
      nodesToWrite
  {----------------
  Helping functions
  ----------------}

  -- read `Node` by `AddressNumber`. From the locals or from the RAM according to the flag
  readByAddressAndFlag addressNumber localFlag = mux (localFlag .==. pure External) externalCase localCase
   where
    externalCase = case sequenceA $ ram addressNumber (pure Nothing) of
      Nothing -> error "There is no Node by this address"
      Just node -> node
    localCase = view containedNode <$> localAddressToLoadedMap addressNumber

  -- write `Node` by the `ActualAddressNumber` in the RAM
  writeByLoadedNode loadedNode =
    ram
      (view originalAddress <$> loadedNode)
      (sequenceA $ Just $ bundle (view originalAddress <$> loadedNode, sequenceA $ Just $ view containedNode <$> loadedNode))
