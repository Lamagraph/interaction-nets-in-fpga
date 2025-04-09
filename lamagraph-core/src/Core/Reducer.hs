{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Core.Reducer where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, (:>))
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node

import Data.Maybe (fromMaybe, isNothing)
import INet.Net
import qualified Prelude as P

toDelta ::
  (KnownNat portsNumber, KnownNat edgesNumber, KnownNat nodesNumber) =>
  ActivePair portsNumber agentType ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber agentType ->
  Delta nodesNumber edgesNumber portsNumber agentType
toDelta acPair (ReduceRuleResult resultEdges resultNodes) =
  Delta
    (nodesActiveReflexivityReduction acPair resultNodes)
    (edgesReduction acPair resultEdges)
    acPair

-- | Make transitive reduction of `Edge`s
edgesReduction ::
  forall portsNumber edgesNumber agentType.
  (KnownNat portsNumber, KnownNat edgesNumber) =>
  ActivePair portsNumber agentType ->
  Vec edgesNumber (Maybe (Edge portsNumber)) ->
  Vec edgesNumber (Maybe (Edge portsNumber))
edgesReduction
  ( ActivePair
      (LoadedNode leftActiveNode leftActiveAddress)
      (LoadedNode rightActiveNode rightActiveAddress)
    )
  fullEdges =
    snd
      $ ifoldl
        ( \(excludeEdges, resEdges) i maybeEdge ->
            let (exEdges, rEdge) = if excludeEdges !! i then (repeat False, Nothing) else edgeProcessing maybeEdge (repeat False)
             in (exEdges `insertVec` excludeEdges, rEdge +>> resEdges)
        )
        (repeat @edgesNumber False, repeat @edgesNumber Nothing)
        fullEdges
   where
    insertVec insertedVec toVec =
      ifoldl
        (\acc i el -> (if el then replace i True acc else acc))
        toVec
        insertedVec
    edgeProcessing ::
      Maybe (Edge portsNumber) ->
      Vec edgesNumber Bool ->
      (Vec edgesNumber Bool, Maybe (Edge portsNumber))
    edgeProcessing edge@(Just (Edge leftConnection rightConnection)) acc =
      case (getActiveNodeByConnection leftConnection, getActiveNodeByConnection rightConnection) of
        (Just leftConnectionTo, Just rightConnectionTo) ->
          case (leftConnectionTo, rightConnectionTo) of
            (NotConnected, NotConnected) -> (acc, Nothing)
            (NotConnected, Connected _) -> handleOneEndEdge leftConnection rightConnectionTo NotConnected
            (Connected _, NotConnected) -> handleOneEndEdge rightConnection leftConnectionTo NotConnected
            (Connected (Port lAddress _), Connected (Port rAddress _)) -> case (isActive lAddress, isActive rAddress) of
              (False, False) -> (acc, Just $ Edge leftConnectionTo rightConnectionTo)
              (False, True) -> oneEndIsActive leftConnection rightConnectionTo
              (True, False) -> oneEndIsActive rightConnection leftConnectionTo
              (True, True) -> case (findNextEnd fullEdges leftConnectionTo, findNextEnd fullEdges rightConnectionTo) of
                (Just (leftNextConnection, leftIndex), Just (rightNextConnection, rightIndex)) ->
                  if acc !! leftIndex || acc !! rightIndex
                    then (acc, Nothing)
                    else
                      edgeProcessing
                        (Just $ Edge leftNextConnection rightNextConnection)
                        (replace leftIndex True (replace rightIndex True acc))
                (_, _) -> (acc, Nothing)
        (Nothing, Just rightConnectionTo) -> case rightConnectionTo of
          NotConnected -> if leftConnection == NotConnected then (acc, Nothing) else (acc, Just $ Edge leftConnection NotConnected)
          Connected _ -> handleOneEndEdge leftConnection rightConnectionTo leftConnection
        (Just leftConnectionTo, Nothing) -> case leftConnectionTo of
          NotConnected -> if rightConnection == NotConnected then (acc, Nothing) else (acc, Just $ Edge NotConnected rightConnection)
          Connected _ -> handleOneEndEdge rightConnection leftConnectionTo rightConnection
        (Nothing, Nothing) -> case (leftConnection, rightConnection) of
          (NotConnected, NotConnected) -> (acc, Nothing)
          (_, _) -> (acc, edge)
     where
      oneEndIsActive fixEnd steppedEnd = case findNextEnd fullEdges steppedEnd of
        Just (connection, i) ->
          if acc !! i then (acc, Nothing) else edgeProcessing (Just $ Edge fixEnd connection) (replace i True acc)
        Nothing -> (acc, Nothing)
      handleOneEndEdge fixedConnection steppedOverActiveConnection@(Connected (Port address _)) externalConnection =
        if isActive address
          then oneEndIsActive fixedConnection steppedOverActiveConnection
          else (acc, Just $ Edge steppedOverActiveConnection externalConnection)
      handleOneEndEdge _ NotConnected _ = error ""
    edgeProcessing Nothing _ = (repeat False, Nothing)
    getActiveNodeByConnection connection =
      ( \(Port address _) ->
          if address == leftActiveAddress
            then findConnection leftActiveNode connection
            else if address == rightActiveAddress then findConnection rightActiveNode connection else Nothing
      )
        =<< connection
    isActive a = a == leftActiveAddress || a == rightActiveAddress

-- | Find adjacent `Connection` and index of it in `Vec` among `Edge`s
findNextEnd ::
  forall n portsNumber.
  (KnownNat n) =>
  Vec n (Maybe (Edge portsNumber)) ->
  Connection portsNumber ->
  Maybe (Connection portsNumber, Index n)
findNextEnd fullEdges connection =
  helper fullEdges connection 0
 where
  helper ::
    forall n1.
    (KnownNat n1) =>
    Vec n1 (Maybe (Edge portsNumber)) ->
    Connection portsNumber ->
    Index n ->
    Maybe (Connection portsNumber, Index n)
  helper allEdges end@(Connected (Port address _)) indexCounter =
    case allEdges of
      Nil -> Nothing
      Cons edge otherEdges ->
        case edge of
          Nothing -> helper otherEdges end (indexCounter + 1)
          Just (Edge leftConnection@(Connected (Port leftAddress _)) rightConnection@(Connected (Port rightAddress _))) ->
            if leftAddress == address
              then Just (rightConnection, indexCounter)
              else if rightAddress == address then Just (leftConnection, indexCounter) else helper otherEdges end (indexCounter + 1)
          Just (Edge NotConnected (Connected (Port a _))) ->
            if a == address then Just (NotConnected, indexCounter) else helper otherEdges end (indexCounter + 1)
          Just (Edge (Connected (Port a _)) NotConnected) -> if a == address then Just (NotConnected, indexCounter) else helper otherEdges end (indexCounter + 1)
          Just (Edge NotConnected NotConnected) -> helper otherEdges end (indexCounter + 1)
  helper _ NotConnected _ = undefined

nodesActiveReflexivityReduction ::
  forall portsNumber nodesNumber agentType.
  (KnownNat portsNumber, KnownNat nodesNumber) =>
  ActivePair portsNumber agentType ->
  Vec nodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
nodesActiveReflexivityReduction
  ( ActivePair
      (LoadedNode leftActiveNode leftActiveAddress)
      (LoadedNode rightActiveNode rightActiveAddress)
    )
  fullNodes = map (fmap nodeProcessing) fullNodes
   where
    -- TODO: think about error messages
    connectionProcessing connection@(Connected (Port a _))
      | a == leftActiveAddress = case findConnection leftActiveNode connection of
          Nothing -> errorX "1"
          Just activeConnection -> fromMaybe (errorX "2") $ findAdjacentConnection fullNodes activeConnection
      | a == rightActiveAddress = case findConnection rightActiveNode connection of
          Nothing -> errorX "3"
          Just pointer -> fromMaybe (errorX "4") $ findAdjacentConnection fullNodes pointer
      | otherwise = connection
    connectionProcessing NotConnected = NotConnected
    nodeProcessing lN@(LoadedNode n@Node{..} _) =
      set
        containedNode
        (set secondaryPorts newSecondaryPorts (set primaryPort newPrimaryPort n))
        lN
     where
      newPrimaryPort = connectionProcessing _primaryPort
      newSecondaryPorts =
        foldl
          (\sPorts connection -> sPorts <<+ (connectionProcessing <$> connection))
          def
          _secondaryPorts

findAdjacentConnection ::
  forall n portsNumber agentType.
  (KnownNat n, KnownNat portsNumber) =>
  Vec n (Maybe (LoadedNode portsNumber agentType)) ->
  Connection portsNumber ->
  Maybe (Connection portsNumber)
findAdjacentConnection fullNodes end = case fullNodes of
  Nil -> Nothing
  Cons node otherNodes -> case node of
    Nothing -> findAdjacentConnection otherNodes end
    Just (LoadedNode n a) ->
      if (n ^. primaryPort) == end
        then Just $ Connected (Port a Primary)
        else case findIndex (maybe False isPointerAtActive) (n ^. secondaryPorts) of
          Nothing -> findAdjacentConnection otherNodes end
          Just i -> Just $ Connected (Port a (Id i))
   where
    -- isPointerAtActive NotConnected = False
    isPointerAtActive x = x == end

-- findAdjacentConnection _ NotConnected = Nothing

{- | Get all `AddressNumber`s of external `Node`s of `ActivePair`, i.e. collect addresses of connected to active pair nodes

__Note__ maxNumOfChangedNodes has to be specified
-}
getInterface ::
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  ActivePair portsNumber agentType ->
  Interface maxNumOfChangedNodes
getInterface (ActivePair (LoadedNode lNode leftAddress) (LoadedNode rNode rightAddress)) =
  foldl
    (\oldInterface maybeConnection -> maybe oldInterface (updateInterfaceByConnection oldInterface) maybeConnection)
    def
    allConnections
 where
  isExternal address = not (address == leftAddress || address == rightAddress)
  allConnections =
    getAllConnections lNode ++ getAllConnections rNode
  updateInterfaceByConnection interface connection = case connection of
    Connected (Port address _) -> if isExternal address && Just address `notElem` interface then Just address +>> interface else interface
    NotConnected -> interface

{- | Give the next root `Node` after reduction that
1) Contains exactly one `NotConnected` `Port`
1) Is the only one
3) Is contained in one and only component of connectivity
-}
changeRootNode ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , Show agentType
  ) =>
  ActivePair portsNumber agentType ->
  AddressNumber ->
  Delta nodesNumber edgesNumber portsNumber agentType ->
  AddressNumber
changeRootNode (ActivePair (LoadedNode _ leftAddress) (LoadedNode _ rightAddress)) oldRootNode delta =
  if oldRootNode == leftAddress || oldRootNode == rightAddress
    then case findInMaybeNodes (delta ^. newNodes) of
      Nothing -> case findInMaybeEdges (delta ^. newEdges) of
        Nothing -> errorX $ "There is must be a root Node in the Net\n" P.++ show delta
        Just a -> a
      Just a -> a
    else oldRootNode
 where
  isNotConnected = isNothing
  nodeHasFreePort (Node primPort secPorts _) = any (maybe False isNotConnected) (Just primPort :> secPorts)
  findInMaybeNodes :: forall n. (KnownNat n) => Vec n (Maybe (LoadedNode portsNumber agentType)) -> Maybe AddressNumber
  findInMaybeNodes = \case
    Nil -> Nothing
    Cons (Just (LoadedNode n a)) ns -> if nodeHasFreePort n then Just a else findInMaybeNodes ns
    Cons Nothing ns -> findInMaybeNodes ns
  findInMaybeEdges :: forall n. (KnownNat n) => Vec n (Maybe (Edge portsNumber)) -> Maybe AddressNumber
  findInMaybeEdges = \case
    Nil -> Nothing
    Cons (Just (Edge leftConnection rightConnection)) es -> case (leftConnection, rightConnection) of
      (NotConnected, NotConnected) -> errorX "Wrong delta: NotConnected -- NotConnected edge"
      (NotConnected, Connected (Port address _)) -> Just address
      (Connected (Port address _), NotConnected) -> Just address
      (Connected _, Connected _) -> findInMaybeEdges es
    Cons Nothing es -> findInMaybeEdges es

-- | Apply reduction rule by `ActivePair` and allocate necessary amount of memory
reduceNoSignal ::
  forall portsNumber nodesNumber edgesNumber cellsNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  ) =>
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  MemoryManager cellsNumber ->
  ActivePair portsNumber agentType ->
  (Delta nodesNumber edgesNumber portsNumber agentType, MemoryManager cellsNumber)
reduceNoSignal chooseReductionRule memoryManager activeP = (delta, writeNewActives delta newMemoryManager)
 where
  leftLoadedNode = view leftNode activeP
  rightLoadedNode = view rightNode activeP
  leftNodeType = view (containedNode . nodeType) leftLoadedNode
  rightNodeType = view (containedNode . nodeType) rightLoadedNode
  reductionRuleInfo = chooseReductionRule leftNodeType rightNodeType
  transitionFunction = view reductionFunction reductionRuleInfo
  (freeAddresses, newMemoryManager) =
    giveAddressesNoSignal (view necessaryAddressesCount reductionRuleInfo) memoryManager
  reduceRuleResult = transitionFunction freeAddresses leftLoadedNode rightLoadedNode
  delta = toDelta activeP reduceRuleResult
