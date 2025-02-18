{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core.Reducer where

import Clash.Prelude
import Control.Lens hiding (Index, (:>))
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node

import INet.Net

toDelta ::
  (KnownNat portsNumber, KnownNat edgesNumber, KnownNat nodesNumber) =>
  ActivePair portsNumber agentType ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber agentType ->
  Delta nodesNumber edgesNumber portsNumber agentType
toDelta acPair (ReduceRuleResult resultEdges resultNodes) = Delta resultNodes resultEdges acPair

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

-- | Apply reduction rule by `ActivePair` and allocate necessary amount of memory
reduce ::
  forall dom portsNumber nodesNumber edgesNumber cellsNumber agentType.
  ( KnownDomain dom
  , KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  ) =>
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  Signal dom (MemoryManager cellsNumber) ->
  Signal dom (ActivePair portsNumber agentType) ->
  (Signal dom (Delta nodesNumber edgesNumber portsNumber agentType), Signal dom (MemoryManager cellsNumber))
reduce chooseReductionRule memoryManager activeP = (toDelta <$> activeP <*> reduceRuleResult, newMemoryManager)
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
