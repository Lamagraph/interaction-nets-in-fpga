{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
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
  deriving (Generic, NFDataX, Show)

$(makeLenses ''ReduceRuleResult)

type ChooseReductionRule nodesNumber edgesNumber portsNumber agentType =
  agentType -> agentType -> ReductionRuleInfo nodesNumber edgesNumber portsNumber agentType

instance Show (ChooseReductionRule nodesNumber edgesNumber portsNumber agentType) where
  show _ = ""

-- | Information about concrete reduction rule: reduction function and count of new nodes
data
  ReductionRuleInfo
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
  , _necessaryAddressesCount :: Index CellsNumber
  }
  deriving (Generic, NFDataX)

instance
  Show
    ( ReductionRuleInfo
        (maxNumOfNewNodes :: Nat)
        (maxNumOfNewEdges :: Nat)
        (portsNumber :: Nat)
        agentType
    )
  where
  show _ = ""

$(makeLenses ''ReductionRuleInfo)

class INet agentType nodesNumber edgesNumber portsNumber where
  -- | A function to determine which reduction rule should be applied and how much memory is required for this
  getReduceRuleInfo ::
    (KnownNat nodesNumber, KnownNat edgesNumber, KnownNat portsNumber) =>
    agentType ->
    agentType ->
    ReductionRuleInfo nodesNumber edgesNumber portsNumber agentType
