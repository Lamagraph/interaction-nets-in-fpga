{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : INet.Define
Description : Worked example: the lambda-calculus agent set written with the EDSL

This module is the example for "INet.Edsl": it declares the agent set
@{Apply, Abstract, Erase, Duplicate}@ with one 'declareAgents' splice, writes
the rules as builder bodies, and wires them into the
@INet AgentSimpleLambda 4 2 2@ instance. That instance is what @topEntity@
synthesizes; design, limits, and the relation to the hand-written reference
set: @docs/edsl.md@.

__Name collision.__ @AgentSimpleLambda@ here is the four-constructor type
(@ApplyLabel@ … @DuplicateLabel@) of the four-address instance. The module
"Core.Concrete.ReduceRulesLambda" declares a /different/ three-constructor
@AgentSimpleLambda@ (@Apply@, @Abstract@, @Erase@) for the two-address
reference instance kept for tests; the two are unrelated and there is no
conversion between them.

== Why this agent set

@Apply@ and @Abstract@ are the arity-2 agents of the lambda encoding: their
principal ports form the active pair, and their two secondary ports carry the
argument and the body. @Erase@ has arity 0 and acts only through its principal
port. @Duplicate@ is an ordinary declared arity-2 agent, not built-in
machinery: its rules use the same primitives as every other rule, and it is
what makes sharing explicit.

== Why the instance ceilings are @4 2 2@

The class order is @INet agentType nodesNumber edgesNumber portsNumber@.

* /4 nodes/ — @dupAnyBody@ creates two copies plus one fan per secondary port
  of the copied agent; the widest copied agent has two secondary ports, so
  @2 + 2 = 4@. Every other rule creates at most two nodes.
* /2 edges/ — an 'Edge' is produced only by an external-to-external wire or a
  free-to-external wire (the latter stays dangling); only beta does that, at
  most twice; erase and duplicate write into fresh-node accumulators and emit
  no edges.
* /2 ports/ — the maximum secondary arity of the set (@Apply@, @Abstract@,
  @Duplicate@); @Erase@ pads both slots with @Nothing@.

The per-pair allocation counts live in the `Rule` types: beta 0,
erase/erase 0, erase-vs-non-erase 2 (one eraser per secondary port of the
other agent), duplicate-vs-@Apply@/@Abstract@ 4. Each count is the number of
addresses the corresponding body actually takes from the pool; it is extracted
from the rule's @freshNodesNumber@ type parameter, not maintained separately.

== Why the rules look the way they do

* /Beta/ connects the two wires leaving the applied function's secondary slots
  to the two leaving the abstraction's, pairwise. The body only zips the port
  vectors, so the mirrored orientation is the same body with the arguments
  swapped; nothing is allocated.
* /Erase/ must not leave the surviving agent's wires dangling: for each of its
  secondary slots the body raises a fresh @Erase@ whose principal port takes
  over the connection when the slot is connected; an unconnected slot yields an
  unconnected eraser. @Erase >< Erase@ has nothing to preserve and annihilates.
  The traversal is generic over the partner arity and uses `mapPorts`, which
  threads the builder state through 'Vec.mapAccumL', so it stays synthesizable
  (no lists, no monadic folds).
* /Duplicate/ copies the other agent twice and, for every secondary slot, adds
  a @Duplicate@ fan: the fan's principal takes over the slot's connection when
  there is one (otherwise it stays unconnected), and its two secondary ports
  reach the matching slots of the copies. The copied label is the
  non-@Duplicate@ half of the pair, so one body serves both orientations.
  Copies and fans are wired entirely through accumulators, hence no external
  edges.
* @Duplicate >< Duplicate@ has no rule on purpose — duplicating a duplicator
  needs a different theory — and dispatch rejects it with @errorX@ before any
  allocation. Every pair containing @Erase@ is an erasure: the table lists the
  canonical @Erase >< X@ orientation, and @Duplicate >< Erase@ resolves to it
  through the mirrored lookup in @ruleFor@ (its padding slots yield dangling
  erasers, as the unit tests pin) rather than copying an eraser.

__Why the instance is generated.__ The generator emits the declaration
skeleton; the rule bodies and the @(pair, rule)@ table carry the semantics.
`INet.Edsl.Dispatch.declareINet` expands the table into @canonicalRule@,
@ruleFor@, and the @INet@ instance, so no dispatch arm or allocation-count
method is written by hand.

== Rule bodies and typed wrappers

The rule implementation is split deliberately:

* `eraseAnyBody`, `dupAnyBody`, and `betaBody` are the /graph rewrites/ — they
  describe which fresh nodes and wires a reduction produces.
* `eraseRule` and `duplicateRule` are /type-level wrappers/. `Rule`'s
  @freshNodesNumber@ is a type parameter, so a plain `RuleBuilder` body cannot
  carry the allocation count. The wrapper fixes that count, instantiates the
  agent singleton (for example @eraseRule Apply@), and passes both pair nodes'
  secondary vectors to the body through `ruleFromSecondaries`.
* `ruleFor` derives the reversed orientation by mirroring; there are no
  @eraseAgainstLeft@ / @duplicateAgainstLeft@ duplicates.

The wrapper carries no graph semantics. Keeping it thin means the body remains
a named, testable rewrite while the type-level bookkeeping stays next to the
`Rule` constructor.

== Pinned by tests

The exact output shapes are fixed by the unit tests under
@tests/Tests/Core/Unit/EdslRulesUnit/@, and the shared rules are compared
against the hand-written set by the property suite. For example, the Apply/Erase
fixture with pool @{6,5}@ reduces to two fresh erasers: @Erase@ at 6 wired to
@2.Id1@ and @Erase@ at 5 wired to @2.Id0@, with no edges (@Erase.hs@).
-}
module INet.Define where

import Clash.Prelude hiding (Index, imap, (:>))
import Clash.Sized.Index (Index)
import qualified Clash.Sized.Vector as Vec
import INet.Edsl
import INet.Edsl.Agents (declareAgents)
import INet.Edsl.Dispatch (declareINet, rule)

-- ====================
-- Agent set: {Apply, Abstract, Erase, Duplicate}
-- ====================

{- | Generate the flat runtime label @AgentSimpleLambda@ (stored in
@Node._nodeType@; not to be confused with the same-named label type of
"Core.Concrete.ReduceRulesLambda", see the module header), the promoted-tag
enum, the kind-indexed arity family, the singleton GADT, and the
@AgentLabel@ instances.
-}
$( declareAgents
     "AgentSimpleLambda"
     [ ("Apply", 2)
     , ("Abstract", 2)
     , ("Erase", 0)
     , ("Duplicate", 2)
     ]
 )

-- ====================
-- Secondary-port indices for arity-2 agents
-- ====================

-- | Typed index for secondary port 0 (avoids numeric defaulting warnings)
sec2Port0 :: Index 2
sec2Port0 = 0

-- | Typed index for secondary port 1
sec2Port1 :: Index 2
sec2Port1 = 1

-- ====================
-- Example rules for {Apply, Abstract, Erase, Duplicate}
-- ====================

{- | Beta: @Apply >< Abstract@.

Connects the two connections leaving the applied function's secondary slots to
the two leaving the abstraction's, pairwise; nothing is created, so the free
pool goes unused. The body only zips the two port vectors, which is why the
mirrored orientation in `ruleFor` reuses it with the arguments swapped.
-}
betaBody :: Vec 2 PortPointer -> Vec 2 PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
betaBody leftPorts rightPorts = do
  wire (leftPorts !! sec2Port0) (rightPorts !! sec2Port0)
  wire (leftPorts !! sec2Port1) (rightPorts !! sec2Port1)

{- | Erase: @Erase >< X@, and also the erasure half of @Duplicate >< Erase@.

One fresh @Erase@ per secondary port of @X@: its principal port takes over the
wire the dying @X@ held, so erasure propagates along every connection instead
of leaving dangling ends; a free or void slot yields a fresh eraser whose
principal stays unconnected. The traversal is generic over the partner arity
and uses `mapPorts`, which threads the builder state through 'Vec.mapAccumL'
without lists or monadic folds, so the body stays synthesizable.
-}
eraseAnyBody ::
  forall partnerArity.
  Vec partnerArity PortPointer ->
  RuleBuilder 2 4 2 AgentSimpleLambda ()
eraseAnyBody ports = mapPorts ports $ \port -> do
  (addr, _) <- fresh Erase
  wire port (Main addr)

{- | @Erase >< Erase@: two erasers annihilate. No nodes, no wires, no pool
addresses; both agents have arity 0, so there is nothing to traverse.
-}
eraseEraseBody :: RuleBuilder 2 4 2 AgentSimpleLambda ()
eraseEraseBody = pure ()

{- | Duplicate: @Duplicate >< X@ for @X@ in @{Apply, Abstract}@.

Creates two copies of @X@ (same label, hence same arity) and one @Duplicate@
fan per secondary port of @X@: the fan's principal takes over the connection
from the original slot (a free or void slot leaves it unconnected), and its
two secondary ports reach the matching slots of the two copies. Everything is
wired inside the accumulators, so the rule emits no external edges. The body
is generic in the copied agent's arity; for this set it takes four pool
addresses: two copies plus two fans. The per-port fan traversal goes through
`mapPorts`, keeping the body in the same synthesizable fixed-vector style as
the erasure body.
-}
dupAnyBody ::
  forall copiedAgentKind.
  (AgentLabel (Agent copiedAgentKind) AgentSimpleLambda) =>
  Agent copiedAgentKind ->
  Vec 2 PortPointer ->
  Vec (AgentArityOf (Agent copiedAgentKind)) PortPointer ->
  RuleBuilder 2 4 2 AgentSimpleLambda ()
dupAnyBody copyAgent branchPorts xPorts = do
  (primA, copyA) <- fresh copyAgent
  (primB, copyB) <- fresh copyAgent
  wire (branchPorts !! sec2Port0) (Main primA)
  wire (branchPorts !! sec2Port1) (Main primB)
  mapPorts (Vec.zip3 xPorts copyA copyB) $ \(xp, ca, cb) -> do
    (primF, fanP) <- fresh Duplicate
    wire xp (Main primF)
    wire (fanP !! sec2Port0) ca
    wire (fanP !! sec2Port1) cb

-- ====================
-- Rules with their exact allocation counts
-- ====================

{- | Beta creates no nodes: the body only connects the two port vectors.
The mirrored orientation is derived by `mirrorSomeRule`; edge order follows the
body's argument order. `ruleFromSecondaries` only feeds the two secondary-port
vectors into the body; the type-level count @0@ is fixed by this signature.
-}
betaRule :: Rule 0 2 4 2 AgentSimpleLambda
betaRule = ruleFromSecondaries betaBody

-- | Two erasers annihilate without creating or wiring anything.
eraseEraseRule :: Rule 0 2 4 2 AgentSimpleLambda
eraseEraseRule = Rule $ \_ _ -> eraseEraseBody

{- | Package the erasure body as a canonical @Erase >< X@ rule.

This is the type-level wrapper around `eraseAnyBody`, not a second graph
rewrite. The body is polymorphic in the partner arity and has no 'Rule' type,
so it cannot carry @freshNodesNumber@; this signature fixes the count to
@AgentArityOf (Agent partnerKind)@ and supplies the concrete singleton. The
reversed pair @X >< Erase@ is derived by `ruleFor` mirroring, so there is no
@eraseAgainstLeft@.
-}
eraseRule ::
  forall partnerKind.
  ( AgentLabel (Agent partnerKind) AgentSimpleLambda
  , AgentArityOf (Agent partnerKind) ~ 2
  ) =>
  Agent partnerKind ->
  Rule (AgentArityOf (Agent partnerKind)) 2 4 2 AgentSimpleLambda
eraseRule _ = ruleFromSecondaries $ \_ rightPorts ->
  eraseAnyBody rightPorts

{- | Package the duplication body as a canonical @Duplicate >< X@ rule.

`dupAnyBody` is the graph rewrite; this wrapper fixes the type-level allocation
count @2 + AgentArityOf (Agent copiedAgentKind)@, instantiates the copied agent
singleton, and hands both nodes' secondary vectors to the body. The wrapper is
intentionally thin: `Rule` needs the type-level count, and the body cannot
provide it. @X >< Duplicate@ is derived by `ruleFor` mirroring.
-}
duplicateRule ::
  forall copiedAgentKind.
  ( AgentLabel (Agent copiedAgentKind) AgentSimpleLambda
  , AgentArityOf (Agent copiedAgentKind) ~ 2
  ) =>
  Agent copiedAgentKind ->
  Rule (2 + AgentArityOf (Agent copiedAgentKind)) 2 4 2 AgentSimpleLambda
duplicateRule copiedAgent =
  ruleFromSecondaries (dupAnyBody copiedAgent)

{- | Declarative rule table: every unordered pair appears once.
`declareINet` expands this list into `canonicalRule`, `ruleFor`, and the
`INet` instance. The reverse orientation is derived by `ruleFor`; a pair with
no entry remains a runtime `errorX`.
-}
$( declareINet
     "AgentSimpleLambda"
     4
     2
     2
     [ rule "Apply" "Abstract" [|betaRule|]
     , rule "Erase" "Erase" [|eraseEraseRule|]
     , rule "Erase" "Apply" [|eraseRule Apply|]
     , rule "Erase" "Abstract" [|eraseRule Abstract|]
     , rule "Erase" "Duplicate" [|eraseRule Duplicate|]
     , rule "Duplicate" "Apply" [|duplicateRule Apply|]
     , rule "Duplicate" "Abstract" [|duplicateRule Abstract|]
     ]
 )
