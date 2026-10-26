{- | Unit tests: erasure of the EDSL rules on fixed fixtures.

`Erase >< X` materializes fresh `Erase` nodes for the partner's secondaries; the
fixtures below pin that against hand-derived literals rather than against
"Core.Concrete.ReduceRulesLambda": a non-Erase actor with an /empty/ secondary
slot makes the EDSL materialize a node for it anyway
(`EndFree, EndFresh -> writeAcc _ NotConnected`), often leaving a permanently
unreachable node whose primary stays `NotConnected`, where the hand-written
rules raise @error "Agent type of the node is incorrect"@.
-}
module Tests.Core.Unit.EdslRulesUnit.Erase (
  eraseTests,
) where

import qualified Clash.Prelude as C
import Core.Node
import qualified INet.Define as Def
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Tests.Core.Unit.Common.Fixtures (pairEpsAppToId, pairEpsAppToIdSimple, pairIdErase)
import Tests.Core.Unit.Common.Normalize (freeVec4)
import Tests.Core.Unit.Common.Rule (edsOf)

{- | \((\lambda x.(\varepsilon)(x))(\lambda y.y)\): the apply's secondaries become
fresh `Erase` nodes at the first two pool addresses.
-}
reduceEpsAppToId :: TestTree
reduceEpsAppToId =
  testCase "erase on (λx.(ε x))(λy.y): two fresh erasers" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = pairEpsAppToId
  (actEdges, actNodes) = edsOf (freeVec4 6 5) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node (Connected (Port 2 (Id 1))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 6
    , LoadedNode (Node (Connected (Port 2 (Id 0))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 5
    ]
  void = Nothing

{- | \((\varepsilon)(\lambda y.y)\): the first apply secondary is empty, so the
first fresh eraser's primary stays `NotConnected`.
-}
reduceEpsAppToIdSimple :: TestTree
reduceEpsAppToIdSimple =
  testCase "erase on (ε)(λy.y): one free eraser primary" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = pairEpsAppToIdSimple
  (actEdges, actNodes) = edsOf (freeVec4 4 3) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node NotConnected (void C.:> void C.:> C.Nil) Def.EraseLabel) 4
    , LoadedNode (Node (Connected (Port 0 Primary)) (void C.:> void C.:> C.Nil) Def.EraseLabel) 3
    ]
  void = Nothing

-- | \(\lambda x.x\) against an eraser: both copies reference the abstract's secondaries.
reduceIdErase :: TestTree
reduceIdErase =
  testCase "erase on λx.x: two fresh erasers on the abstract's children" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = pairIdErase
  (actEdges, actNodes) = edsOf (freeVec4 3 2) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node (Connected (Port 0 (Id 1))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 3
    , LoadedNode (Node (Connected (Port 0 (Id 0))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 2
    ]
  void = Nothing

eraseTests :: TestTree
eraseTests =
  testGroup
    "erase"
    [ reduceEpsAppToId
    , reduceEpsAppToIdSimple
    , reduceIdErase
    ]
