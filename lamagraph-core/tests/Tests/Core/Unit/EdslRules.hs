{- | Property tests: EDSL rules ("INet.Define") must match the hand-written
rules ("Core.Concrete.ReduceRulesLambda") on the shared agent set.

The hand-written module knows only @{Apply, Abstract, Erase}@ and returns a
smaller result (`ReduceRuleResult 2 2 2`); the EDSL returns `ReduceRuleResult
4 2 2` with extra `Nothing` padding. Comparison normalizes both sides to
projected lists ("Tests.Core.Unit.Common.Normalize"), so only real content is
compared.

Generators produce non-trivial inputs: a random connected sub-net of 4-8
nodes with distinct addresses, from which an active pair is drawn. The pair's
secondary ports reference *open* ports of the enclosing net — never a busy or
interface port: an Erase actor (arity 0) carries padding `Nothing` slots, and
every slot targets a distinct port of a node outside the pair (pair-member
ports are excluded: primary is the pair wire, secondaries are the pair
interface). In the full net each such wire's target port holds the mirror
connection back to the pair slot, so every wire has matched endpoints. The
free-address pool is drawn from the complement, so it is distinct from every
address the net references. Referencing a to-be-freed address from a live
port — or reusing a free address twice — is an invalid net, and the EDSL
(`slotByAddr`) and the hand-written rules disagree on such inputs by design.

Two further limits of this property, by construction rather than by accident:

* Every secondary slot of a non-Erase actor is generated /connected/. The
  hand-written rules raise `error "Agent type of the node is incorrect"` on an
  empty slot ("Core.Concrete.ReduceRulesLambda"), so the `NotConnected`-slot
  path is not differentially testable at all; it is pinned against literals in
  "Tests.Core.Unit.EdslRulesUnit" instead.
* @Erase >< Erase@ is a single input up to the ignored primaries: Erase has
  arity 0 and `eraseEraseBody` is `pure ()`. The property confirms the dispatch
  arm exists and allocates nothing; there is nothing else to vary.

The projections `normEdges` \/ `normNodes` compare ordered lists, which pins the
allocation and wiring order the rule bodies happen to use. That is intentional —
it is deterministic — but reordering the `fresh` calls inside a rule body will
fail these tests without changing semantics.
-}
module Tests.Core.Unit.EdslRules where

import Clash.Prelude (Vec)
import qualified Clash.Sized.Vector as Vec
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified INet.Define as Def
import INet.Net (INet (makeReduction), ReduceRuleResult)
import Test.Tasty
import Test.Tasty.Hedgehog
import Prelude

import Tests.Core.Unit.Common.Normalize (freeVec2, freeVec4, mkNode, normEdges, normNodesDef, normNodesRW, toDef)

-- ====================
-- Generators: random sub-nets
-- ====================

{- | A random fixture: an active pair inside a connected sub-net, plus the two
free addresses the reduction may allocate. Secondary ports of the pair
reference real nodes of the enclosing net (never the free pool).
-}
data SubnetFixture = SubnetFixture
  { sfLeftAddr :: AddressNumber
  , sfRightAddr :: AddressNumber
  , sfLeftSecs :: Vec 2 (Maybe (Connection 2))
  , sfRightSecs :: Vec 2 (Maybe (Connection 2))
  , sfFree0 :: AddressNumber
  , sfFree1 :: AddressNumber
  }
  deriving (Show)

-- | Address of a port/active-pair node (never equals a free address)
genAddr :: H.Gen AddressNumber
genAddr = fromIntegral <$> Gen.integral (Range.linear 0 15)

-- | Free-pool address, distinct from every address already in use
genAddrNotIn :: [AddressNumber] -> H.Gen AddressNumber
genAddrNotIn used = Gen.filter (`notElem` used) genAddr

-- | @n@ pairwise-distinct addresses
genDistinct :: Int -> H.Gen [AddressNumber]
genDistinct 0 = pure []
genDistinct n = do
  x <- genAddr
  xs <- Gen.filter (notElem x) (genDistinct (n - 1))
  pure (x : xs)

vec2 :: [a] -> Vec 2 a
vec2 = Vec.unsafeFromList

-- | Does this label have secondary ports? (Erase has arity 0)
hasSecs :: RW.AgentSimpleLambda -> Bool
hasSecs = (/= RW.Erase)

{- | @n@ distinct elements drawn from @pool@ (pairwise-distinct by construction)

A short pool would make @take n@ under-deliver, which would silently corrupt
the @take nLeft@ \/ @drop nLeft@ split of the caller's slots instead of
failing. Unreachable with the sizes @genActorSecs@ currently uses (at most 4
slots against at least 6 candidate ports), so this is a guard against future
edits to the generator rather than a reachable case.
-}
genNDistinct :: [a] -> Int -> H.Gen [a]
genNDistinct pool n
  | length pool < n = error ("genNDistinct: pool of " ++ show (length pool) ++ " too small for " ++ show n)
  | otherwise = take n <$> Gen.shuffle pool

{- | Secondary ports of the active pair, arity-aware: every slot of a
non-Erase actor holds a real connection into an *open* context port; an Erase
actor's slots are padding `Nothing`. Targets are pairwise-distinct and always
refer to a node outside the pair — pair members are never referenced, because
their primary is busy with the pair wire and their secondary ports *are* the
pair interface (each already owns one wire); a wire's target port holds the
mirror connection back to the referencing slot. @actors@ are the two pair
addresses, @others@ the rest of the net.
-}
genActorSecs ::
  RW.AgentSimpleLambda ->
  RW.AgentSimpleLambda ->
  AddressNumber ->
  AddressNumber ->
  [AddressNumber] ->
  H.Gen (Vec 2 (Maybe (Connection 2)), Vec 2 (Maybe (Connection 2)))
genActorSecs lLbl rLbl lAddr rAddr otherAddrs = do
  let ctxPorts = [(a, p) | a <- otherAddrs, p <- [Primary, Id 0, Id 1]]
      nLeft = if hasSecs lLbl then 2 else 0
      nRight = if hasSecs rLbl then 2 else 0
      nTotal = nLeft + nRight
  k <- Gen.int (Range.linear (if nTotal == 0 then 0 else 1) nTotal)
  extPorts <- genNDistinct ctxPorts k
  restPorts <- genNDistinct (filter (`notElem` extPorts) ctxPorts) (nTotal - k)
  let targets = extPorts ++ restPorts
      conn (a, p) = Just (Connected (Port a p))
      slotConns = map conn targets
      leftConns = take nLeft slotConns ++ replicate (2 - nLeft) Nothing
      rightConns = drop nLeft slotConns ++ replicate (2 - nRight) Nothing
  pure (vec2 leftConns, vec2 rightConns)

genSubnetFixture :: RW.AgentSimpleLambda -> RW.AgentSimpleLambda -> H.Gen SubnetFixture
genSubnetFixture lLbl rLbl = do
  size <- Gen.int (Range.linear 4 8)
  addrs <- genDistinct size
  (leftAddr, rightAddr) <- genActorPair addrs
  let actorAddrs = [leftAddr, rightAddr]
      otherAddrs = filter (`notElem` actorAddrs) addrs
  (secsL, secsR) <- genActorSecs lLbl rLbl leftAddr rightAddr otherAddrs
  free0 <- genAddrNotIn addrs
  free1 <- genAddrNotIn (free0 : addrs)
  pure (SubnetFixture leftAddr rightAddr secsL secsR free0 free1)
 where
  genActorPair adds = do
    a <- Gen.element adds
    b <- Gen.filter (/= a) (Gen.element adds)
    pure (a, b)

-- ====================
-- Hand-written dispatch
-- ====================

{- | Symmetric dispatch over the hand-written rules, mirroring the EDSL `INet`
instance (one rule is asymmetric: `Erase` must be on the left)
-}
hardwiredApply ::
  Vec 2 (Maybe AddressNumber) ->
  LoadedNode 2 RW.AgentSimpleLambda ->
  LoadedNode 2 RW.AgentSimpleLambda ->
  ReduceRuleResult 2 2 2 RW.AgentSimpleLambda
hardwiredApply freeAddrs lN rN = case (nodeLabel lN, nodeLabel rN) of
  (RW.Erase, RW.Erase) -> RW.eraseToErase freeAddrs lN rN
  (RW.Erase, _) -> RW.eraseToAbstractOrApplyRule freeAddrs lN rN
  (_, RW.Erase) -> RW.eraseToAbstractOrApplyRule freeAddrs rN lN
  (RW.Apply, RW.Abstract) -> RW.applyToLambdaRule freeAddrs lN rN
  (RW.Abstract, RW.Apply) -> RW.applyToLambdaRule freeAddrs rN lN
  other -> error ("no hand-written rule for pair " ++ show other)
 where
  nodeLabel = _nodeType . _containedNode

-- ====================
-- Properties
-- ====================

propEdslMatchesHardwired :: RW.AgentSimpleLambda -> RW.AgentSimpleLambda -> H.Property
propEdslMatchesHardwired rwLbl rwRbl = H.property $ do
  f <- H.forAll (genSubnetFixture rwLbl rwRbl)
  let lN_RW = mkNode rwLbl (sfLeftSecs f) (sfLeftAddr f) (sfRightAddr f)
      rN_RW = mkNode rwRbl (sfRightSecs f) (sfRightAddr f) (sfLeftAddr f)
      lN_D = mkNode (toDef rwLbl) (sfLeftSecs f) (sfLeftAddr f) (sfRightAddr f)
      rN_D = mkNode (toDef rwRbl) (sfRightSecs f) (sfRightAddr f) (sfLeftAddr f)
      hard :: ReduceRuleResult 2 2 2 RW.AgentSimpleLambda
      hard = hardwiredApply (freeVec2 (sfFree0 f) (sfFree1 f)) lN_RW rN_RW
      eds :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
      eds = makeReduction (freeVec4 (sfFree0 f) (sfFree1 f)) lN_D rN_D
  normEdges hard H.=== normEdges eds
  normNodesRW hard H.=== normNodesDef eds

edslRulesTests :: TestTree
edslRulesTests =
  testGroup
    "EDSL rules vs hand-written rules"
    [ testProperty "Apply >< Abstract" (propEdslMatchesHardwired RW.Apply RW.Abstract)
    , testProperty "Abstract >< Apply" (propEdslMatchesHardwired RW.Abstract RW.Apply)
    , testProperty "Erase >< Erase" (propEdslMatchesHardwired RW.Erase RW.Erase)
    , testProperty "Erase >< Apply" (propEdslMatchesHardwired RW.Erase RW.Apply)
    , testProperty "Erase >< Abstract" (propEdslMatchesHardwired RW.Erase RW.Abstract)
    , testProperty "Apply >< Erase" (propEdslMatchesHardwired RW.Apply RW.Erase)
    , testProperty "Abstract >< Erase" (propEdslMatchesHardwired RW.Abstract RW.Erase)
    ]
