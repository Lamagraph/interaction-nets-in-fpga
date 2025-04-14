{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}

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

class INet agentType nodesNumber edgesNumber portsNumber where
  getAddressesAllocationCount :: agentType -> agentType -> Index CellsNumber

  makeReduction ::
    Vec nodesNumber (Maybe AddressNumber) -> -- vec of free addresses
    LoadedNode portsNumber agentType ->
    LoadedNode portsNumber agentType ->
    ReduceRuleResult nodesNumber edgesNumber portsNumber agentType
