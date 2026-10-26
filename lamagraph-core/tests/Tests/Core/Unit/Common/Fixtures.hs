{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | Active pairs extracted from the hand-built nets of "Core.Concrete.Initial".

The EDSL rule tests and the hand-written-rule tests need the same two
`LoadedNode`s out of a fixture net, so the positional pattern matches live here
instead of being repeated at every call site.
-}
module Tests.Core.Unit.Common.Fixtures (
  Pair,
  mkActivePair,
  pairIdApplyToId,
  pairLoopEdge,
  pairEpsAppToId,
  pairEpsAppToIdRoot,
  pairEpsAppToIdSimple,
  pairIdErase,
) where

import qualified Clash.Prelude as C
import qualified Core.Concrete.Initial as Init
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda)
import Core.MemoryManager.MemoryManager (ActivePair (ActivePair))
import Core.Node
import Prelude

-- | The two nodes of an active pair, in the order the fixture stores them.
type Pair = (LoadedNode 2 AgentSimpleLambda, LoadedNode 2 AgentSimpleLambda)

-- | Wrap a `Pair` into the `ActivePair` the reducer and memory manager take.
mkActivePair :: Pair -> ActivePair 2 AgentSimpleLambda
mkActivePair = uncurry ActivePair

-- | \((\lambda x.x)(\lambda y.y)\): pair = Apply\@0 and the inner Abstract\@2.
pairIdApplyToId :: Pair
pairIdApplyToId =
  let (Just p0 C.:> _ C.:> Just p2 C.:> _) = Init.initialIdApplyToId
   in (p0, p2)

-- | Self-looped apply\/abstract: every secondary references its own node.
pairLoopEdge :: Pair
pairLoopEdge =
  let (Just p0 C.:> Just p1 C.:> _) = Init.initialLoopEdge
   in (p0, p1)

{- | \((\lambda x.(\varepsilon)(x))(\lambda y.y)\): pair = Apply\@3 and Erase\@4
(the `Apply >< Erase` redex of the net).
-}
pairEpsAppToId :: Pair
pairEpsAppToId =
  let (_ C.:> _ C.:> _ C.:> Just p3 C.:> Just p4 C.:> _) = Init.initialEpsAppToId
   in (p3, p4)

{- | The pair the `initialEpsAppToId` fixture's `MemoryManager` points at:
Apply\@1 and Abstract\@2, i.e. the redex a fetch from the active-address map
finds first. Note this is not the pair the EDSL erase tests use — they take
Apply\@3 \/ Erase\@4 (`pairEpsAppToId`).
-}
pairEpsAppToIdRoot :: Pair
pairEpsAppToIdRoot =
  let (_ C.:> Just p1 C.:> Just p2 C.:> _) = Init.initialEpsAppToId
   in (p1, p2)

-- | \((\varepsilon)(\lambda y.y)\): pair = Apply\@1 and Erase\@2.
pairEpsAppToIdSimple :: Pair
pairEpsAppToIdSimple =
  let (_ C.:> Just p1 C.:> Just p2 C.:> _) = Init.initialEpsAppToIdSimple
   in (p1, p2)

-- | \(\lambda x.x\) connected to an eraser: pair = Abstract\@0 and Erase\@1.
pairIdErase :: Pair
pairIdErase =
  let (Just p0 C.:> Just p1 C.:> _) = Init.initialIdErase
   in (p0, p1)
