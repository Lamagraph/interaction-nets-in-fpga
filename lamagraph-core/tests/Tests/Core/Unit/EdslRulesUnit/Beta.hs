{- | Unit tests: β-reduction of the EDSL rules on fixed fixtures.

Covers the four `Apply >< Abstract` shapes: a plain β-pair, the mirrored
dispatch (`Abstract >< Apply`), a self-looped pair, and a fully-connected pair.
Expected values are baked in as literals; where the hand-written rules express
the same semantics at rule level the result is additionally pinned against
"Core.Concrete.ReduceRulesLambda".

A wire whose pair-slot end is `NotConnected` (e.g. an abstract whose secondary
is empty) becomes a dangling `Edge NotConnected _` on the other side: the
external port is now free. That edge is what `Core.Reducer.changeRootNode`
uses to find the successor root when the old root was a pair member — see
`reduceIdAppIdBeta` below.
-}
module Tests.Core.Unit.EdslRulesUnit.Beta (
  betaTests,
) where

import qualified Clash.Prelude as C
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Tests.Core.Unit.Common.Fixtures (pairIdApplyToId, pairLoopEdge)
import Tests.Core.Unit.Common.Normalize (freeVec2, freeVec4)
import Tests.Core.Unit.Common.Rule (edsOf, rwEdgesOf)

-- | A fully-connected β-pair: all four secondaries hold real connections.
fixtureBetaFull :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureBetaFull =
  ( LoadedNode (Node (Connected (Port 1 Primary)) lSecs RW.Apply) 2
  , LoadedNode (Node (Connected (Port 2 Primary)) rSecs RW.Abstract) 1
  )
 where
  lSecs = Just (Connected (Port 4 Primary)) C.:> Just (Connected (Port 5 (Id 1))) C.:> C.Nil
  rSecs = Just (Connected (Port 6 (Id 1))) C.:> Just (Connected (Port 7 (Id 0))) C.:> C.Nil

{- | β on \((\lambda x.x)(\lambda y.y)\): the argument wire plus the dangling
edge for the empty `Apply` secondary (see module header).
-}
reduceIdAppIdBeta :: TestTree
reduceIdAppIdBeta =
  testCase "beta on id-apply-to-id: argument wire plus dangling edge" $
    (actEdges @?= expectedEdges) >> (actNodes @?= [])
 where
  (actEdges, actNodes) = edsOf (freeVec4 3 4) pairIdApplyToId
  expectedEdges =
    [ Edge NotConnected (Connected (Port 2 (Id 1)))
    , Edge (Connected (Port 1 Primary)) (Connected (Port 2 (Id 0)))
    ]

{- | Same net mirrored: `Abstract >< Apply` dispatch path. The dispatch
normalizes pair orientation (`betaBody rp lp`), so the resulting wire list is
identical to the un-mirrored case.
-}
reduceIdAppIdBetaMirrored :: TestTree
reduceIdAppIdBetaMirrored =
  testCase "beta on id-apply-to-id mirrored (Abstract >< Apply)" $
    (actEdges @?= expectedEdges) >> (actNodes @?= [])
 where
  (rN, lN) = pairIdApplyToId
  (actEdges, actNodes) = edsOf (freeVec4 3 4) (lN, rN)
  expectedEdges =
    [ Edge NotConnected (Connected (Port 2 (Id 1)))
    , Edge (Connected (Port 1 Primary)) (Connected (Port 2 (Id 0)))
    ]

{- | Both secondaries self-looped: both pairs become external wires.
Pinned against the hand-written rule: identical result.
-}
reduceLoopEdgeBeta :: TestTree
reduceLoopEdgeBeta =
  testCase "beta on self-looped apply/abstract: two wires" $
    (actEdges @?= expectedEdges) >> (rwEdges @?= actEdges) >> (actNodes @?= [])
 where
  (lN, rN) = pairLoopEdge
  (actEdges, actNodes) = edsOf (freeVec4 2 3) (lN, rN)
  expectedEdges =
    [ Edge (Connected (Port 0 (Id 1))) (Connected (Port 1 (Id 1)))
    , Edge (Connected (Port 0 (Id 0))) (Connected (Port 1 (Id 0)))
    ]
  rwEdges = rwEdgesOf (freeVec2 2 3) lN rN

-- | Fully-connected β-pair: all four secondaries become wires; pinned against the hand-written rule.
reduceBetaFull :: TestTree
reduceBetaFull =
  testCase "beta on fully-connected pair: both argument wires" $
    (actEdges @?= expectedEdges) >> (rwEdges @?= actEdges) >> (actNodes @?= [])
 where
  (lN, rN) = fixtureBetaFull
  (actEdges, actNodes) = edsOf (freeVec4 8 9) (lN, rN)
  expectedEdges =
    [ Edge (Connected (Port 4 Primary)) (Connected (Port 6 (Id 1)))
    , Edge (Connected (Port 5 (Id 1))) (Connected (Port 7 (Id 0)))
    ]
  rwEdges = rwEdgesOf (freeVec2 8 9) lN rN

betaTests :: TestTree
betaTests =
  testGroup
    "beta"
    [ reduceIdAppIdBeta
    , reduceIdAppIdBetaMirrored
    , reduceLoopEdgeBeta
    , reduceBetaFull
    ]
