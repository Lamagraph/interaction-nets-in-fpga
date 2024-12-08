{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module Core.Reducer where

import Clash.Prelude
import Control.Lens
import Core.Node

type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3

data Edge (portsNumber :: Nat) = Edge
  { _leftEnd :: Connection portsNumber
  , _rightEnd :: Connection portsNumber
  }
  deriving (Generic, NFDataX, Show, Eq)

$(makeLenses ''Edge)

data ActivePair (portsNumber :: Nat) = ActivePair
  { _leftNode :: LoadedNode portsNumber
  , _rightNode :: LoadedNode portsNumber
  }
  deriving (Show, Eq, Generic, NFDataX, Bundle)
$(makeLenses ''ActivePair)

-- | Result of abstract reduction rule
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber))
  }

$(makeLenses ''ReduceRuleResult)

data Delta (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = Delta
  { _newNodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber))
  , _newEdges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _activePair :: ActivePair portsNumber
  }
  deriving (Show, Eq, Generic, NFDataX)

$(makeLenses ''Delta)

type Interface externalNodesNumber = Vec externalNodesNumber (Maybe AddressNumber)

toDelta ::
  (KnownNat portsNumber, KnownNat edgesNumber, KnownNat nodesNumber) =>
  ActivePair portsNumber ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber ->
  Delta nodesNumber edgesNumber portsNumber
toDelta acPair reduceResult = Delta (reduceResult ^. nodes) (reduceResult ^. edges) acPair

reducer ::
  forall dom portsNumber nodesNumber edgesNumber.
  (KnownDomain dom, KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber) =>
  (Node portsNumber -> Node portsNumber -> ReduceRuleResult nodesNumber edgesNumber portsNumber) ->
  Signal dom (ActivePair portsNumber) ->
  Signal dom (Delta nodesNumber edgesNumber portsNumber)
reducer transFunction activeP = toDelta <$> activeP <*> reduceRuleRes
 where
  leftLNode = (^. leftNode) <$> activeP
  rightLNode = (^. rightNode) <$> activeP
  reduceRuleRes = transFunction <$> (view containedNode <$> leftLNode) <*> (view containedNode <$> rightLNode)
