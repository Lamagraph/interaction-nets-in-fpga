{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core.Reducer where

import Clash.Prelude
import Control.Lens hiding (Index)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import INet.Net

-- | Result of abstract reduction rule
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber))
  }

$(makeLenses ''ReduceRuleResult)

type ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber =
  Agent -> Agent -> ReductionRuleInfo cellsNumber nodesNumber edgesNumber portsNumber

-- | Information about  concrete reduction rule: reduction function and count of new nodes
data ReductionRuleInfo cellsNumber maxNumOfNewNodes maxNumOfNewEdges portsNumber
  = ReduceFunctionInfo
  { _reductionFunction ::
      Vec maxNumOfNewNodes (Maybe AddressNumber) ->
      LoadedNode portsNumber ->
      LoadedNode portsNumber ->
      ReduceRuleResult maxNumOfNewNodes maxNumOfNewEdges portsNumber
  , _necessaryAddressesCount :: Index cellsNumber
  }

$(makeLenses ''ReductionRuleInfo)

toDelta ::
  (KnownNat portsNumber, KnownNat edgesNumber, KnownNat nodesNumber) =>
  ActivePair portsNumber ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber ->
  Delta nodesNumber edgesNumber portsNumber
toDelta acPair reduceResult = Delta (reduceResult ^. nodes) (reduceResult ^. edges) acPair

{- | Get all `AddressNumber`s of external `Node`s of `ActivePair`, i.e. collect addresses of connected to active pair nodes

__Note__ maxNumOfChangedNodes have to be specified
-}
getInterface ::
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  ActivePair portsNumber ->
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

-- | Apply reduction rule by `ActivePair` and allocate necessary amount of memory
reducer ::
  forall dom portsNumber nodesNumber edgesNumber cellsNumber.
  ( KnownDomain dom
  , KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  ) =>
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber ->
  Signal dom (MemoryManager cellsNumber) ->
  Signal dom (ActivePair portsNumber) ->
  (Signal dom (Delta nodesNumber edgesNumber portsNumber), Signal dom (MemoryManager cellsNumber))
reducer chooseReductionRule memoryManager activeP = (toDelta <$> activeP <*> reduceRuleResult, newMemoryManager)
 where
  leftLoadedNode = view leftNode <$> activeP
  rightLoadedNode = view rightNode <$> activeP
  leftNodeType = view (containedNode . nodeType) <$> leftLoadedNode
  rightNodeType = view (containedNode . nodeType) <$> rightLoadedNode
  reductionRuleInfo = chooseReductionRule <$> leftNodeType <*> rightNodeType
  transitionFunction = view reductionFunction <$> reductionRuleInfo
  (freeAddresses, newMemoryManager) =
    giveAddresses (view necessaryAddressesCount <$> reductionRuleInfo) memoryManager
  reduceRuleResult = transitionFunction <*> freeAddresses <*> leftLoadedNode <*> rightLoadedNode
