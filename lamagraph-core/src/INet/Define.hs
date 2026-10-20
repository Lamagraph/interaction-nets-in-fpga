{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Usage of the EDSL for the lambda-calculus agent set {Apply, Abstract, Erase, Duplicate}.

Defines the runtime labels, rule bodies, and the @INet AgentSimpleLambda 4 2 2@
instance. The generic EDSL machinery lives in "INet.Edsl".

__Note__ the name @AgentSimpleLambda@ is used twice in the project for two
/different/ types: this module declares it with four constructors
(@ApplyLabel@, @AbstractLabel@, @EraseLabel@, @DuplicateLabel@) for the
4-address-per-pair instance, while "Core.Concrete.ReduceRulesLambda" declares
its own three-constructor @AgentSimpleLambda@ (@Apply@, @Abstract@, @Erase@)
for the 2-address instance that @topEntity@ uses. They are unrelated types
with no conversions between them.
-}
module INet.Define where

import Clash.Prelude hiding (Index, imap, (:>))
import Clash.Sized.Index (Index)
import qualified Clash.Sized.Vector as Vec
import Control.Lens hiding (Index, imap, (:>))
import Control.Monad.State (runState, state)
import Core.Node
import INet.Edsl
import INet.Net (INet (..), ReduceRuleResult)

-- ====================
-- Runtime labels for the lambda-calculus agent set
-- ====================

-- | Flat runtime label stored in `Node._nodeType`.
-- Not to be confused with the same-named label type of "Core.Concrete.ReduceRulesLambda" (see the module header).
data AgentSimpleLambda = ApplyLabel | AbstractLabel | EraseLabel | DuplicateLabel
  deriving (Generic, NFDataX, Show, Eq, ShowX)

-- | Map singleton agent to runtime label (decisions.md D-Agent)
instance AgentLabel 'ApplyKind AgentSimpleLambda where agentLabel _ = ApplyLabel

instance AgentLabel 'AbstractKind AgentSimpleLambda where agentLabel _ = AbstractLabel

instance AgentLabel 'EraseKind AgentSimpleLambda where agentLabel _ = EraseLabel

instance AgentLabel 'DuplicateKind AgentSimpleLambda where agentLabel _ = DuplicateLabel

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

{- | beta rule: `Apply >< Abstract` wires the two secondary pairs
-- [AI-WIP] beta rule body in the new EDSL
-}
betaBody :: Vec 2 PortPointer -> Vec 2 PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
betaBody leftPorts rightPorts = do
  wire (leftPorts !! sec2Port0) (rightPorts !! sec2Port0)
  wire (leftPorts !! sec2Port1) (rightPorts !! sec2Port1)

{- | erase rule: @Erase >< X@ — one fresh `Erase` per secondary port of @X@
Iterates over @X@'s secondary ports with `Vec.mapAccumL`, threading the builder
state purely (synthesizable: no lists, no monadic folding).
-}
eraseAnyBody :: forall n. Vec n PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
eraseAnyBody ports = state $ \initState ->
  let st' = fst (Vec.mapAccumL step initState ports)
   in ((), st')
 where
  step :: RuleState 2 4 2 AgentSimpleLambda -> PortPointer -> (RuleState 2 4 2 AgentSimpleLambda, ())
  step st port = let (a, st') = runState (fan port) st in (st', a)
  fan :: PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
  fan port = do
    (addr, _) <- fresh Erase
    wire port (Main addr)

{- | `Erase >< Erase`: nothing arises
-- [AI-WIP] erase-erase rule body in the new EDSL
-}
eraseEraseBody :: RuleBuilder 2 4 2 AgentSimpleLambda ()
eraseEraseBody = pure ()

{- | duplicator rule: @Duplicate >< X@ — two copies of @X@, one fan `Duplicate` per secondary port of @X@
Iterates over @X@'s secondary ports with `Vec.mapAccumL`, threading the builder
state purely (synthesizable: no lists, no monadic folding).
-}
dupAnyBody ::
  forall a.
  (AgentLabel a AgentSimpleLambda) =>
  Agent a ->
  Vec 2 PortPointer ->
  Vec (AgentArity a) PortPointer ->
  RuleBuilder 2 4 2 AgentSimpleLambda ()
dupAnyBody copyAgent branchPorts xPorts = do
  (primA, copyA) <- fresh copyAgent
  (primB, copyB) <- fresh copyAgent
  wire (branchPorts !! sec2Port0) (Main primA)
  wire (branchPorts !! sec2Port1) (Main primB)
  state $ \initState ->
    let st' = fst (Vec.mapAccumL step initState (Vec.zip3 xPorts copyA copyB))
     in ((), st')
 where
  step ::
    RuleState 2 4 2 AgentSimpleLambda ->
    (PortPointer, PortPointer, PortPointer) ->
    (RuleState 2 4 2 AgentSimpleLambda, ())
  step st (xp, ca, cb) = let (a, st') = runState (fan xp ca cb) st in (st', a)
  fan :: PortPointer -> PortPointer -> PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
  fan xp ca cb = do
    (primF, fanP) <- fresh Duplicate
    wire xp (Main primF)
    wire (fanP !! sec2Port0) ca
    wire (fanP !! sec2Port1) cb

-- ====================
-- INet instance
-- ====================

instance INet AgentSimpleLambda 4 2 2 where
  getAddressesAllocationCount :: AgentSimpleLambda -> AgentSimpleLambda -> Index CellsNumber
  getAddressesAllocationCount left right = case (left, right) of
    (ApplyLabel, AbstractLabel) -> 0
    (AbstractLabel, ApplyLabel) -> 0
    (EraseLabel, EraseLabel) -> 0
    (EraseLabel, _) -> 2
    (_, EraseLabel) -> 2
    (DuplicateLabel, DuplicateLabel) -> errorX "No rule for this pair"
    (DuplicateLabel, _) -> 4
    (_, DuplicateLabel) -> 4
    _ -> errorX "No rule for this pair"

  makeReduction ::
    Vec 4 (Maybe AddressNumber) ->
    LoadedNode 2 AgentSimpleLambda ->
    LoadedNode 2 AgentSimpleLambda ->
    ReduceRuleResult 4 2 2 AgentSimpleLambda
  makeReduction freeAddrs lN rN = case (lType, rType) of
    (ApplyLabel, AbstractLabel) -> runReduce freeAddrs (betaBody lp rp)
    (AbstractLabel, ApplyLabel) -> runReduce freeAddrs (betaBody rp lp)
    (EraseLabel, EraseLabel) -> runReduce freeAddrs eraseEraseBody
    (EraseLabel, _) -> runReduce freeAddrs (eraseAnyBody rp)
    (_, EraseLabel) -> runReduce freeAddrs (eraseAnyBody lp)
    (DuplicateLabel, DuplicateLabel) -> errorX "No rule for this pair"
    (DuplicateLabel, ApplyLabel) -> runReduce freeAddrs (dupAnyBody Apply lp rp)
    (ApplyLabel, DuplicateLabel) -> runReduce freeAddrs (dupAnyBody Apply rp lp)
    (DuplicateLabel, AbstractLabel) -> runReduce freeAddrs (dupAnyBody Abstract lp rp)
    (AbstractLabel, DuplicateLabel) -> runReduce freeAddrs (dupAnyBody Abstract rp lp)
    _ -> errorX "No rule for this pair"
   where
    lType = lN ^. containedNode . nodeType
    rType = rN ^. containedNode . nodeType
    lp = nodeSecondaries lN
    rp = nodeSecondaries rN
