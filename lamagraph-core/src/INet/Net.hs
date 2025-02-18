{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module INet.Net where

import Clash.Prelude
import Control.Lens (makeLenses)
import Core.Node

-- | Result of abstract reduction rule
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) agentType = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  }

$(makeLenses ''ReduceRuleResult)

type ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType =
  agentType -> agentType -> ReductionRuleInfo cellsNumber nodesNumber edgesNumber portsNumber agentType

-- | Information about concrete reduction rule: reduction function and count of new nodes
data
  ReductionRuleInfo
    (cellsNumber :: Nat)
    (maxNumOfNewNodes :: Nat)
    (maxNumOfNewEdges :: Nat)
    (portsNumber :: Nat)
    agentType
  = ReduceFunctionInfo
  { _reductionFunction ::
      Vec maxNumOfNewNodes (Maybe AddressNumber) -> -- vec of free addresses
      LoadedNode portsNumber agentType ->
      LoadedNode portsNumber agentType ->
      ReduceRuleResult maxNumOfNewNodes maxNumOfNewEdges portsNumber agentType
  , _necessaryAddressesCount :: Index cellsNumber
  }

$(makeLenses ''ReductionRuleInfo)

class INet agentType cellsNumber nodesNumber edgesNumber portsNumber where
  -- | A function to determine which reduction rule should be applied and how much memory is required for this
  getReduceRuleInfo ::
    (KnownNat cellsNumber, KnownNat nodesNumber, KnownNat edgesNumber, KnownNat portsNumber) =>
    agentType ->
    agentType ->
    ReductionRuleInfo cellsNumber nodesNumber edgesNumber portsNumber agentType
