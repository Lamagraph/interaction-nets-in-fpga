{-# HLINT ignore "Functor law" #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core.Reducer where

import Clash.Prelude
import Control.Lens (makeLenses, view, (^.))
import Core.MemoryManager
import Core.Node

type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3

-- | Result of abstract reduction rule
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LocalNode portsNumber))
  }

$(makeLenses ''ReduceRuleResult)

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
