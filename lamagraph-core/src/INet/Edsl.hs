{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : INet.Edsl
Description : Embedded DSL for writing reduction rules as monadic rule bodies

A small builder DSL that turns a declarative wiring description of a
reduction rule into the @ReduceRuleResult@ the generic reducer expects.
A rule body runs in `RuleBuilder` — @State@ over a fixed-size @RuleState@
(all vectors sized by the instance parameters, no dynamic allocation):
`fresh` allocates a new agent at the next free address, `wire` connects two
port ends (external-to-external connections become @Edge@s, everything else
is written straight into the fresh nodes' accumulators), and `runReduce`
materializes the state. Overflow of the fixed vectors is an explicit
`errorX`, never silent truncation.

"INet.Define" instantiates this DSL for the lambda-calculus agent set; the
decision records referenced in the section headers live in
@docs/decisions.md@.
-}
module INet.Edsl (
  -- * Agent kinds (decisions.md D-Agent, D-AgentArity)
  AgentKind (..),
  AgentArity,
  Agent (..),
  AgentLabel (..),

  -- * Port pointers (decisions.md D-Wire)
  PortPointer (..),
  connectionToPointer,
  nodeSecondaries,

  -- * Builder state (decisions.md D-Builder, D-Vec)
  NodeAccumulator (..),
  RuleState (..),
  RuleBuilder,
  initSecAcc,

  -- * Endpoints and port resolution
  EndPoint (..),
  slotByAddr,
  resolveEnd,

  -- * Primitives
  writeAcc,
  addWire,
  wire,
  fresh,

  -- * Materialization
  accToNode,
  materialize,
  runReduce,
) where

import Clash.Prelude
import Control.Monad.State (State, execState, get, gets, modify, put)
import Core.Node
import INet.Net

-- ====================
-- Agent kinds (decisions.md D-Agent, D-AgentArity)
-- ====================

-- | Type-level tag for an agent kind
data AgentKind = ApplyKind | AbstractKind | EraseKind | DuplicateKind
  deriving (Generic, Show, Eq, NFDataX, ShowX)

-- | Type-level secondary-port arity of an agent
type family AgentArity (k :: AgentKind) :: Nat where
  AgentArity 'ApplyKind = 2
  AgentArity 'AbstractKind = 2
  AgentArity 'EraseKind = 0
  AgentArity 'DuplicateKind = 2

-- | Singleton value per agent kind, used as the `fresh` argument
data Agent (k :: AgentKind) where
  Apply :: Agent 'ApplyKind
  Abstract :: Agent 'AbstractKind
  Erase :: Agent 'EraseKind
  Duplicate :: Agent 'DuplicateKind

-- | Class mapping singleton agent to runtime label (decisions.md D-Agent)
class (KnownNat (AgentArity k)) => AgentLabel k l | k -> l where
  -- | Convert singleton to runtime label
  agentLabel :: Agent k -> l

-- ====================
-- Port references
-- ====================

-- | Reference to a port end: a head-binder port of the active pair, or a port of a fresh node
data PortPointer
  = Free
  | Main AddressNumber
  | Pointer AddressNumber Int

-- | Convert a `Connection` into the pointer vocabulary of the DSL (`NotConnected` becomes `Free`).
connectionToPointer :: (KnownNat portsNumber) => Connection portsNumber -> PortPointer
connectionToPointer = \case
  NotConnected -> Free
  Connected (Port a Primary) -> Main a
  Connected (Port a (Id x)) -> Pointer a (fromEnum x)

-- | Extract the active-pair node's secondary ports as `PortPointer`s (head binders)
nodeSecondaries :: (KnownNat portsNumber) => LoadedNode portsNumber l -> Vec portsNumber PortPointer
nodeSecondaries (LoadedNode Node{..} _) = map (maybe Free connectionToPointer) _secondaryPorts

-- ====================
-- Builder state (decisions.md D-Builder)
-- ====================

-- | Accumulated connections for one fresh node, before materialization
data NodeAccumulator (portsNumber :: Nat) = NodeAccumulator
  { _primeAcc :: Connection portsNumber
  , _secAcc :: Vec portsNumber (Maybe (Connection portsNumber))
  }

{- | Rule-builder state: fixed-size vectors bound at the instance level
(decisions.md D-Vec). Wires accumulate edges; `_agents` accumulates
(address, label, connections) per fresh node in creation order.
-}
data RuleState (edgesNumber :: Nat) (nodesNumber :: Nat) (portsNumber :: Nat) (l :: Type) = RuleState
  { _wires :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _agents :: Vec nodesNumber (Maybe (AddressNumber, l, NodeAccumulator portsNumber))
  , _freeAddrs :: Vec nodesNumber (Maybe AddressNumber)
  , _nextWire :: Index edgesNumber
  , _nextAgent :: Index nodesNumber
  }

-- | Monad running one rule body
type RuleBuilder edgesNumber nodesNumber portsNumber l =
  State (RuleState edgesNumber nodesNumber portsNumber l)

-- ====================
-- Primitives (decisions.md D-Fresh, D-Wire)
-- ====================

-- | Initialize a fresh node's secondary slots: real slots explicitly `NotConnected`, padding slots `Nothing`
initSecAcc :: forall portsNumber. (KnownNat portsNumber) => Integer -> Vec portsNumber (Maybe (Connection portsNumber))
initSecAcc arity =
  imap (\j _ -> if j < fromInteger arity then Just NotConnected else Nothing) (repeat Nothing)

-- | Resolved end of a `PortPointer` inside the builder
data EndPoint (portsNumber :: Nat) (nodesNumber :: Nat)
  = EndFree
  | EndExternal (Connection portsNumber)
  | EndFresh (Index nodesNumber) (IdOfPort portsNumber) AddressNumber

-- | Find the fresh-node slot owning the given address, if any
slotByAddr ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat nodesNumber) =>
  AddressNumber ->
  RuleBuilder edgesNumber nodesNumber portsNumber l (Maybe (Index nodesNumber))
slotByAddr a = findIndex isAddr <$> gets _agents
 where
  isAddr (Just (addr, _, _)) = addr == a
  isAddr Nothing = False

{- | Resolve a `PortPointer` against the builder state: `Free` stays free, an
address already claimed by a fresh node resolves to that node's slot
(`EndFresh`), anything else is an external end (`EndExternal`).
-}
resolveEnd ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat nodesNumber, KnownNat portsNumber) =>
  PortPointer ->
  RuleBuilder edgesNumber nodesNumber portsNumber l (EndPoint portsNumber nodesNumber)
resolveEnd Free = pure EndFree
resolveEnd (Main a) = do
  mSlot <- slotByAddr a
  pure $ case mSlot of
    Just slot -> EndFresh slot Primary a
    Nothing -> EndExternal (Just $ Port a Primary)
resolveEnd (Pointer a i) = do
  mSlot <- slotByAddr a
  pure $ case mSlot of
    Just slot -> EndFresh slot (Id $ toEnum i) a
    Nothing -> EndExternal (Just $ Port a (Id $ toEnum i))

-- | Write a connection into a fresh node's accumulator at the given port
writeAcc ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat portsNumber, KnownNat nodesNumber) =>
  Index nodesNumber ->
  IdOfPort portsNumber ->
  Connection portsNumber ->
  RuleBuilder edgesNumber nodesNumber portsNumber l ()
writeAcc slot pid conn = modify $ \s ->
  case _agents s !! slot of
    Nothing -> errorX "writeAcc: no fresh agent at slot"
    Just (addr, lbl, NodeAccumulator primeAcc secAcc) ->
      let acc' = case pid of
            Primary -> NodeAccumulator conn secAcc
            Id i -> NodeAccumulator primeAcc (replace i (Just conn) secAcc)
       in s{_agents = replace slot (Just (addr, lbl, acc')) (_agents s)}

-- | Append an edge to the wire vector; overflow is an explicit error, never silent truncation
addWire ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat edgesNumber) =>
  Edge portsNumber ->
  RuleBuilder edgesNumber nodesNumber portsNumber l ()
addWire e = do
  s <- get
  let i = _nextWire s
  case _wires s !! i of
    Just _ -> errorX "addWire: too many wires for the fixed Vec"
    Nothing -> put s{_wires = replace i (Just e) (_wires s), _nextWire = i + 1}

-- | Connect two port ends (decisions.md D-Wire)
wire ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat edgesNumber, KnownNat nodesNumber, KnownNat portsNumber) =>
  PortPointer ->
  PortPointer ->
  RuleBuilder edgesNumber nodesNumber portsNumber l ()
wire p1 p2 = do
  e1 <- resolveEnd p1
  e2 <- resolveEnd p2
  case (e1, e2) of
    (EndFree, EndFree) -> pure ()
    (EndFree, EndExternal _) -> pure ()
    (EndFree, EndFresh slot pid _) -> writeAcc slot pid NotConnected
    (EndExternal _, EndFree) -> pure ()
    (EndFresh slot pid _, EndFree) -> writeAcc slot pid NotConnected
    (EndExternal c1, EndExternal c2) -> addWire (Edge c1 c2)
    (EndExternal c, EndFresh slot pid _) -> writeAcc slot pid c
    (EndFresh slot pid _, EndExternal c) -> writeAcc slot pid c
    (EndFresh s1 pid1 a1, EndFresh s2 pid2 a2) -> do
      writeAcc s1 pid1 (Just $ Port a2 pid2)
      writeAcc s2 pid2 (Just $ Port a1 pid1)

-- | Create a fresh node; returns its address and bindable secondary ports (decisions.md D-Fresh)
fresh ::
  forall k l e n p.
  (AgentLabel k l, KnownNat (AgentArity k), KnownNat p, KnownNat n) =>
  Agent k ->
  RuleBuilder e n p l (AddressNumber, Vec (AgentArity k) PortPointer)
fresh agent = do
  s <- get
  let i = _nextAgent s
  case _agents s !! i of
    Just _ -> errorX "fresh: too many fresh agents for the fixed Vec"
    Nothing -> do
      let addr = case _freeAddrs s !! i of
            Just freeAddr -> freeAddr
            Nothing -> errorX "fresh: free address pool exhausted"
          lbl = agentLabel agent
          arity = natVal (SNat @(AgentArity k))
          acc = NodeAccumulator NotConnected (initSecAcc arity)
          ports = imap (\j _ -> Pointer addr (fromIntegral j)) (repeat ())
      put s{_agents = replace i (Just (addr, lbl, acc)) (_agents s), _nextAgent = i + 1}
      pure (addr, ports)

-- ====================
-- Materialization (decisions.md D-Builder)
-- ====================

-- | Turn an accumulator into a full @Node@
accToNode :: l -> NodeAccumulator portsNumber -> Node portsNumber l
accToNode lbl NodeAccumulator{..} =
  Node
    { _nodeType = lbl
    , _primaryPort = _primeAcc
    , _secondaryPorts = _secAcc
    }

-- | Materialize builder state into the reduction result
materialize ::
  RuleState edgesNumber nodesNumber portsNumber l ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber l
materialize RuleState{..} = ReduceRuleResult _wires createdNodes
 where
  createdNodes = map (\case Nothing -> Nothing; Just (addr, lbl, acc) -> Just (LoadedNode (accToNode lbl acc) addr)) _agents

-- | Run a rule body over free addresses and materialize the result
runReduce ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat edgesNumber, KnownNat nodesNumber, KnownNat portsNumber) =>
  Vec nodesNumber (Maybe AddressNumber) ->
  RuleBuilder edgesNumber nodesNumber portsNumber l () ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber l
runReduce freeAddrs body = materialize (execState body initState)
 where
  initState :: RuleState edgesNumber nodesNumber portsNumber l
  initState =
    RuleState
      { _wires = repeat Nothing
      , _agents = repeat Nothing
      , _freeAddrs = freeAddrs
      , _nextWire = 0
      , _nextAgent = 0
      }
