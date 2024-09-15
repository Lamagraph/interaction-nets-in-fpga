{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Core.Reducer where

import Clash.Prelude
import Core.Node

type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3
type IdOfPort = Unsigned 3

data ReducerStatus
  = Work
  | Finished
  | ErrorHandleSingleNode
  | ErrorEmptyLeftNode
  deriving (Generic, NFDataX)

-- | State of the reducer (in terms of mealy automaton).
data ReducerState
  = Initial Address
  | Empty
  | OneNodeToLoad
  | Done
  | Failed
  deriving (Generic, NFDataX)

data EdgeEnd = EdgeEnd
  { _addressOfVertex :: Address
  , _idOfPort :: IdOfPort
  }
  deriving (Generic, NFDataX)

data Edge = Edge
  { _leftEnd :: EdgeEnd
  , _rightEnd :: EdgeEnd
  }
  deriving (Generic, NFDataX)

data DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts = DataToStore
  { _nodes :: Vec maxNumOfNodesToStore (Maybe (LoadedNode numberOfPorts))
  , _edges :: Vec maxNumOfEdgesToStore (Maybe Edge)
  }
  deriving (Generic, NFDataX)

-- | Select next left `Node` to handle and next `Address` of right `Node` to be loaded.
selectNextLeftNode ::
  (KnownNat maxNumOfNodesToStore, KnownNat numberOfPorts, KnownNat maxNumOfEdgesToStore) =>
  DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts ->
  ( Maybe (LoadedNode numberOfPorts, Address)
  , DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts
  )
selectNextLeftNode preDataToStore =
  case selectedNode of
    Nothing -> (Nothing, preDataToStore)
    Just (i, node, addressOfNodeToLoad) ->
      ( Just (node, addressOfNodeToLoad)
      , DataToStore (replace i Nothing $ _nodes preDataToStore) (_edges preDataToStore)
      )
 where
  handleNode s i mbNode =
    mbNode
      >>= ( \node -> case selectAddressToLoad node of
              Nothing -> s
              Just a -> Just (i, node, a)
          )
  selectedNode = ifoldl handleNode Nothing $ _nodes preDataToStore

-- | Do one step of reduction and get info about next possible step.
handle ::
  forall
    (maxNumOfNodesToStore :: Nat)
    (maxNumOfEdgesToStore :: Nat)
    (numberOfPorts :: Nat).
  (KnownNat maxNumOfNodesToStore, KnownNat numberOfPorts, KnownNat maxNumOfEdgesToStore) =>
  -- | Left `Node` to handle.
  LoadedNode numberOfPorts ->
  -- | Right `Node` to handle.
  Maybe (LoadedNode numberOfPorts) ->
  -- | Next possible Node and Address to handle and result data.
  (Maybe (LoadedNode numberOfPorts, Address), DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts)
handle leftNode rightNode =
  selectNextLeftNode $ DataToStore (markAllInnerEdges nodes) emptyEdges
 where
  emptyNodes = repeat Nothing :: Vec maxNumOfNodesToStore _
  emptyEdges = repeat Nothing :: Vec maxNumOfEdgesToStore _
  nodes = case rightNode of
    Nothing -> Just leftNode +>> emptyNodes
    Just _ ->
      -- if isActive leftNode n
      rightNode +>> Just leftNode +>> emptyNodes -- reduction rules applied here

-- | Type to indicate that all ports that could be handled locally are over.
data NonVisitedOver = No | Yes

-- | Transition function in mealy automaton.
mealyFunction ::
  ( KnownNat maxNumOfNodesToStore
  , KnownNat maxNumOfEdgesToStore
  , KnownNat numberOfPorts
  ) =>
  -- | State of reducer and automaton.
  ReducerState ->
  -- | Input.
  ( -- Left and right `Node` to reduce.
    (Maybe (LoadedNode numberOfPorts), Maybe (LoadedNode numberOfPorts))
  , ReducerStatus
  , Maybe Address
  -- Address from memory manager if reducer cannot find next `Address` locally.
  ) ->
  -- | (state, output).
  ( ReducerState
  , ( Maybe (DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts)
    , ReducerStatus
    , Maybe (LoadedNode numberOfPorts)
    , -- Next left `Node`
      Maybe Address
    , -- Next `Address` to load
      NonVisitedOver
    )
  )
mealyFunction
  state
  ( (mbLeftNode, mbLoadedNode)
    , status
    , addressOfNodeWithUnvisitedEdges
    ) =
    case state of
      Initial addressOfFirstNodeToLoad -> (Empty, (Nothing, Work, Nothing, Just addressOfFirstNodeToLoad, No))
      Empty ->
        case mbLoadedNode of
          Nothing ->
            (Failed, (Nothing, ErrorEmptyLeftNode, Nothing, Nothing, No))
          Just loadedNode ->
            case addressToLoad of
              Just _ ->
                (OneNodeToLoad, (Nothing, Work, Just loadedNode, addressToLoad, No))
              Nothing ->
                case mbLeftNode of
                  Just leftNode' ->
                    case infoAboutNextNodes of
                      Just (_, _) ->
                        (Failed, (Nothing, ErrorHandleSingleNode, Nothing, Nothing, No))
                      Nothing ->
                        (Empty, (Just dataToStore, Work, Nothing, addressOfNodeWithUnvisitedEdges, Yes))
                   where
                    (infoAboutNextNodes, dataToStore) = handle leftNode' Nothing
                  Nothing ->
                    (Failed, (Nothing, ErrorEmptyLeftNode, Nothing, Nothing, No))
           where
            addressToLoad = selectAddressToLoad loadedNode
      OneNodeToLoad ->
        case mbLeftNode of
          Just leftNode ->
            case infoAboutNextNodes of
              Just (nextLeftNode, nextAddressToLoad) ->
                (OneNodeToLoad, (Just dataToStore, Work, Just nextLeftNode, Just nextAddressToLoad, No))
              Nothing ->
                (Empty, (Just dataToStore, Work, Nothing, addressOfNodeWithUnvisitedEdges, Yes))
           where
            (infoAboutNextNodes, dataToStore) = handle leftNode mbLoadedNode
          Nothing -> (Failed, (Nothing, ErrorEmptyLeftNode, Nothing, Nothing, No))
      Failed -> (Failed, (Nothing, status, Nothing, Nothing, No))
      Done -> (Done, (Nothing, Finished, Nothing, Nothing, No))

-- | Reducer function. Steps over Interaction Net and apply reduction rules until is possible.
reducer ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat maxNumOfNodesToStore
  , KnownNat maxNumOfEdgesToStore
  , KnownNat numberOfPorts
  ) =>
  -- | Address of first `Node` to load.
  Address ->
  -- | Function to load `Node` from RAM.
  (Signal dom (Maybe Address) -> Signal dom (Maybe (LoadedNode numberOfPorts))) ->
  -- | Function that can give next possible `Address` to reduce.
  --   It will come from memory manager which can see all Interaction Net.
  (Signal dom NonVisitedOver -> Signal dom (Maybe Address)) ->
  Signal dom (Maybe (DataToStore maxNumOfNodesToStore maxNumOfEdgesToStore numberOfPorts), ReducerStatus)
reducer addressOfFirstNodeToLoad nodeLoader nonVisitedNodesProvider =
  bundle (dataToStore, actualReducerStatus)
 where
  (dataToStore, actualReducerStatus, nextLeftNode, addressOfNextNodeToLoad, isNonVisitedNodeUsed) = unbundle mealyOutput
  mealyOutput =
    mealy mealyFunction (Initial addressOfFirstNodeToLoad) mealyInput
  mealyInput =
    bundle (bundle (leftNode, loadedNode), status, nonVisitedNodesProvider isNonVisitedNodeUsed)
  loadedNode = register Nothing (nodeLoader addressOfNextNodeToLoad)
  leftNode = register Nothing nextLeftNode
  status = register Work actualReducerStatus
