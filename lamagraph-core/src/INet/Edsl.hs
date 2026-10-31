{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : INet.Edsl
Description : Embedded DSL for writing reduction rules as monadic rule bodies

An embedded language for interaction-net reduction rules. A rule body is a
`RuleBuilder` action — a `State` computation over `RuleState` — that names the
fresh agents it creates and the wires it adds; `runReduce` materializes the
final state into the `ReduceRuleResult` that "INet.Net" hands to
"Core.Reducer". Only the graph shape stays in the source: addresses, vector
padding and `LoadedNode` wrapping are handled here; the mirrored orientation
of a pair is a dispatch choice in the usage module.

== Design

The module is generic over the agent set. An agent is a singleton /type tag/
(the argument of `fresh`), not a constructor carrying ports, so its secondary
arity is a type-level number and `fresh` can return a secondary-port vector of
the right length. The set's labels, kind tags, arity family and singleton GADT
are generated once by "INet.Edsl.Agents"; this module only needs the
`AgentLabel` class.

The builder state is a set of fixed-size `Vec`s whose sizes are the `INet`
instance parameters (one ceiling per agent set, not per rule): an edge vector,
a fresh-node vector in creation order, and the free-address pool.
Accumulating a node's connections before building the `Node` mirrors the
update-side accumulator in "Core.MemoryManager.ChangesAccumulator". Every
write past a vector's capacity is an explicit `errorX`; it is a runtime abort,
not a compile-time check.

== Using it

A body reads the pair's /secondary/ ports with `nodeSecondaries`, creates
nodes with `fresh`, connects ends with `wire`, and returns @()@; the `INet`
instance receives the pool and dispatches on the pair. "INet.Define" is a
worked example, @docs/edsl.md@ is the guide, and @docs/history.md@ records the
design history.
-}
module INet.Edsl (
  -- * Agent labels
  AgentLabel (..),

  -- * Port pointers
  PortPointer (..),
  connectionToPointer,
  nodeSecondaries,

  -- * Builder state
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
-- Agent labels
-- ====================

{- | Class mapping an agent singleton to its runtime label and exposing its
type-level secondary arity.

Indexing by the singleton type (rather than by the promoted tag) keeps this
module free of any concrete agent set: the generator in "INet.Edsl.Agents"
emits one instance per agent, bridging @AgentArityOf@ to the kind-indexed
arity family of that set (e.g. @AgentArityOf (Agent 'ApplyKind) = AgentArity
'ApplyKind@). The functional dependency @s -> l@ fixes one runtime label type
per set. Keeping the arity in the type is what lets `fresh` size the returned
port vector from the declaration instead of trusting a term-level number.
-}
class (KnownNat (AgentArityOf s)) => AgentLabel (s :: Type) l | s -> l where
  -- | Type-level secondary-port arity of the agent singleton
  type AgentArityOf s :: Nat

  -- | Convert singleton to runtime label
  agentLabel :: s -> l

-- ====================
-- Port references
-- ====================

{- | One end of a wire inside a rule body.

@Free@ means \"no end to connect\": a padding or free slot of the pair, or an
end the body deliberately leaves dangling. @Main a@ addresses the principal
port of node @a@; @Pointer a i@ its secondary port @i@. Node @a@ is either an
outside node reached through a pair slot — then the pointer came from
`nodeSecondaries` — or a node created by this body — then it came from
`fresh`.

The secondary index is an 'Int', not @Index portsNumber@, because pair ports
and fresh ports have different arity types. `nodeSecondaries` and `fresh`
produce it in range; `resolveEnd` does not validate a hand-built pointer.
-}
data PortPointer
  = Free
  | Main AddressNumber
  | Pointer AddressNumber Int

{- | Convert a `Connection` into a `PortPointer`. A missing connection is
`Free`; the distinction between a void slot (outer `Nothing`) and an existing
free slot (@Just NotConnected@) is lost here on purpose: neither can be wired.
-}
connectionToPointer :: (KnownNat portsNumber) => Connection portsNumber -> PortPointer
connectionToPointer = \case
  NotConnected -> Free
  Connected (Port a Primary) -> Main a
  Connected (Port a (Id x)) -> Pointer a (fromEnum x)

{- | The pair node's secondary slots as pointers — the head binders of a rule.

Principal ports are not exposed: the pair's principal ports are wired to each
other (that is what makes it active) and are consumed by the reduction, so
binding them again would double-wire a port. Slot contents map as: void slot
and free slot to `Free`, @Connected (Port a Primary)@ to @Main a@,
@Connected (Port a (Id j))@ to @Pointer a j@. The pointers name the /far/
ends of the pair's wires, not the pair's own slots.
-}
nodeSecondaries :: (KnownNat portsNumber) => LoadedNode portsNumber l -> Vec portsNumber PortPointer
nodeSecondaries (LoadedNode Node{..} _) = map (maybe Free connectionToPointer) _secondaryPorts

-- ====================
-- Builder state
-- ====================

{- | Connections collected for one fresh node before the node is built.
`_primeAcc` holds the principal connection, `_secAcc` the secondary slots; a
slot beyond the agent's arity stays `Nothing` (void). `accToNode` turns this
into a `Node` once the body finishes, so a node can be wired before it exists.
-}
data NodeAccumulator (portsNumber :: Nat) = NodeAccumulator
  { _primeAcc :: Connection portsNumber
  , _secAcc :: Vec portsNumber (Maybe (Connection portsNumber))
  }

{- | Builder state of one rule body.

Every vector is sized by the `INet` instance — @edgesNumber@, @nodesNumber@,
@portsNumber@ — so a body allocates nothing dynamically and each phase has a
static bound.

* @_wires@ — external-to-external edges in creation order;
* @_agents@ — fresh nodes as @(address, runtime label, accumulator)@ in
  creation order, padded with `Nothing`;
* @_freeAddrs@ — the pool handed to `runReduce`, consumed left to right;
* @_nextWire@ / @_nextAgent@ — next free slot; writing into an occupied slot
  is an `errorX`.

The sizes are upper bounds over all rules of the set, so an individual body
pads with `Nothing`. Stating the maximum once at the instance avoids
type-level count arithmetic inside @do@ blocks; the price is that a body
overflowing the ceiling is caught at runtime, not at compile time.
-}
data RuleState (edgesNumber :: Nat) (nodesNumber :: Nat) (portsNumber :: Nat) (l :: Type) = RuleState
  { _wires :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _agents :: Vec nodesNumber (Maybe (AddressNumber, l, NodeAccumulator portsNumber))
  , _freeAddrs :: Vec nodesNumber (Maybe AddressNumber)
  , _nextWire :: Index edgesNumber
  , _nextAgent :: Index nodesNumber
  }

{- | Monad of one rule body: `State` over `RuleState`. The @()@ result is the
point — the produced graph is the state. Most bodies need only `fresh` and
`wire`, plus pure `Vec` traversals when a rule fans out over all secondary
ports of an agent.
-}
type RuleBuilder edgesNumber nodesNumber portsNumber l =
  State (RuleState edgesNumber nodesNumber portsNumber l)

-- ====================
-- Primitives
-- ====================

{- | Secondary slots of a fresh node: the first @arity@ slots are real and
start @Just NotConnected@, the rest are padding `Nothing`. @arity@ is an
'Integer' because callers pass @natVal (SNat \@(AgentArityOf s))@ directly;
the arity itself is a type-level declaration, not a runtime field.
-}
initSecAcc :: forall portsNumber. (KnownNat portsNumber) => Integer -> Vec portsNumber (Maybe (Connection portsNumber))
initSecAcc arity =
  imap (\j _ -> if j < fromInteger arity then Just NotConnected else Nothing) (repeat Nothing)

{- | A `PortPointer` resolved against the builder state.

@EndFree@ — nothing to connect. @EndExternal c@ — a port outside the body (a
pair member's slot or any other live node); @c@ is the connection that must be
written on the other side. @EndFresh slot pid a@ — a port of a node created by
this body: its accumulator slot, its port id, and its address, which is needed
to write the mirror connection.
-}
data EndPoint (portsNumber :: Nat) (nodesNumber :: Nat)
  = EndFree
  | EndExternal (Connection portsNumber)
  | EndFresh (Index nodesNumber) (IdOfPort portsNumber) AddressNumber

{- | Find the slot of the fresh node that owns the given address, if any.

`PortPointer` carries only an address, so this linear scan is the link between
a pointer and a node created earlier in the same body. It scans the
@nodesNumber@ node slots (bounded by the instance; four in "INet.Define").
-}
slotByAddr ::
  forall edgesNumber nodesNumber portsNumber l.
  (KnownNat nodesNumber) =>
  AddressNumber ->
  RuleBuilder edgesNumber nodesNumber portsNumber l (Maybe (Index nodesNumber))
slotByAddr a = gets (findIndex isAddr . _agents)
 where
  isAddr (Just (addr, _, _)) = addr == a
  isAddr Nothing = False

{- | Resolve a `PortPointer` against the builder state: `Free` stays free; an
address already claimed by a node created in this body becomes `EndFresh`;
every other address is an external end — an open port of the surrounding net.
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

{- | Write one connection into a fresh node's accumulator: the principal port
overwrites @_primeAcc@, a secondary port replaces that slot. Called by `wire`
whenever an end belongs to a fresh node; a `Nothing` slot here means the body
never created that node.
-}
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

{- | Append an external edge to the wire vector.

Edge order follows the order of `wire` calls and is observable in the result,
so tests pin it. Overflow of the fixed vector is an explicit `errorX`, never
silent truncation.
-}
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

{- | Connect two port ends.

The effect follows from the resolved endpoints, and the two ends are not
symmetric in how they are recorded:

* free with free, or free with an external end — no-op; a missing end is
  elided rather than turned into a dangling `Edge`;
* free with a fresh node's port — mark that fresh port @NotConnected@; the
  node still materializes, but the end stays unwired (this is how an empty
  partner slot leaves a fresh eraser or fan unconnected);
* external with external — append an `Edge`;
* external with a fresh node's port — write the external connection into the
  fresh node's accumulator;
* fresh with fresh — write both mirror connections, one into each accumulator.

Writing a fresh/fresh link in both directions is what keeps the net's wiring
symmetric (the convention is documented in "Core.Node"). Pair slots enter a
body as pointers to the far ends of their wires, so wiring two non-`Free`
pointers produces an external edge, as beta does; a well-formed input net
never points a pair slot back into the pair.
-}
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

{- | Create a fresh node of the given agent kind.

The @i@-th creation takes the @i@-th pool address (creation order defines
address assignment), claims the next node slot, initializes the accumulator
(principal @NotConnected@, secondary slots from `initSecAcc`), and returns the
address together with the node's secondary ports. The address is needed to
attach a principal port (@Main addr@); the returned vector is arity-typed
(@AgentArityOf s@), so fan-out code derives its length from the declaration.

Creation is deliberately separate from connection: `fresh` only allocates and
returns handles, while `wire` connects them. One node can therefore be wired
at its principal port and at any subset of its secondary ports without a
combined \"create and attach\" primitive.

Aborts with `errorX` when the fixed node vector is full or when the pool has
no address left for this slot.
-}
fresh ::
  forall s l e n p.
  (AgentLabel s l, KnownNat (AgentArityOf s), KnownNat p, KnownNat n) =>
  s ->
  RuleBuilder e n p l (AddressNumber, Vec (AgentArityOf s) PortPointer)
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
          arity = natVal (SNat @(AgentArityOf s))
          acc = NodeAccumulator NotConnected (initSecAcc arity)
          ports = imap (\j _ -> Pointer addr (fromIntegral j)) (repeat ())
      put s{_agents = replace i (Just (addr, lbl, acc)) (_agents s), _nextAgent = i + 1}
      pure (addr, ports)

-- ====================
-- Materialization
-- ====================

{- | Attach the runtime label to an accumulator and build the `Node`; the
address is added by `materialize`.
-}
accToNode :: l -> NodeAccumulator portsNumber -> Node portsNumber l
accToNode lbl NodeAccumulator{..} =
  Node
    { _nodeType = lbl
    , _primaryPort = _primeAcc
    , _secondaryPorts = _secAcc
    }

{- | Build the reduction result: copy the edge vector and turn every occupied
node slot into a `LoadedNode` carrying its address. Unused slots stay
`Nothing`, as the fixed-size interface requires.
-}
materialize ::
  RuleState edgesNumber nodesNumber portsNumber l ->
  ReduceRuleResult nodesNumber edgesNumber portsNumber l
materialize RuleState{..} = ReduceRuleResult _wires createdNodes
 where
  createdNodes = map (\case Nothing -> Nothing; Just (addr, lbl, acc) -> Just (LoadedNode (accToNode lbl acc) addr)) _agents

{- | Run a rule body over a free-address pool and materialize the result.

The pool has one entry per node slot; `Nothing` entries are unused budget, and
`fresh` consumes entries left to right. The caller must pass addresses that are
disjoint from every address the input net references, and each address is used
at most once. The result is padded to the instance's @nodesNumber@ /
@edgesNumber@ because those sizes are in the type. This function does not
check the body against the ceiling — the fixed vectors do, with `errorX` on
the first write past capacity.
-}
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
