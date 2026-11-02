{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{- |
Module      : Core.CPU
Description : The reduction finite-state machine

A synthesizable state machine that drives one interaction-net reduction at a
time against the net RAM: it fetches an active pair, applies the reduction
rule via "Core.Reducer.reduce", rewrites the affected external nodes and
writes the fresh nodes back, then loops. All memory traffic goes out as
`RamForm` transactions on `_ramForm`; the node fetched for an earlier request
returns on the input (@CPUIn@).

The FSM proper is `step`; `mealyCore` wraps it into a Clash @mealyS@ circuit.
The wiring around it — RAM, input register — lives in "Core.Core". The
phase table and the cycle budget per phase are described in the repository's
@docs/architecture.md@ (\"CPU finite-state machine\"); the transition rules
are documented on `Phase` and `step` below.

Timing note: RAM read data for a request issued in cycle @t@ is seen by the
FSM two cycles later (synchronous RAM plus the input register), and each
request is driven on `_ramForm` in two consecutive cycles so the delayed
enable path of "Core.Loader.delayedMaybeRam" stays stable — that is what the
@Delay*@ phases of `Phase` are for.
-}
module Core.CPU where

import Clash.Prelude
import Control.Lens hiding (Index)
import Control.Monad.State.Strict
import Core.Loader
import Core.Map
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe (fromMaybe, isJust)
import INet.Net
import qualified Prelude as P

-- | One cycle of CPU input: the node fetched by an earlier `RamForm` request (`Nothing` when nothing valid was requested two cycles ago).
newtype CPUIn portsNumber agentType = CPUIn (Maybe (LoadedNode portsNumber agentType))
  deriving (Generic, NFDataX, Show)

{- | One cycle of CPU output: the RAM transaction of this cycle (read address
plus optional write payload) and the address of the root node the computation
currently stands on.
-}
data CPUOut portsNumber agentType = CPUOut
  { _ramForm :: Maybe (RamForm portsNumber agentType)
  , _nextRootNodeAddress :: AddressNumber
  }
  deriving (Generic, ShowX, Eq, Show, NFDataX)

-- | Output for phases without memory traffic: no transaction, root address unchanged.
defaultOut :: AddressNumber -> CPUOut portsNumber agentType
defaultOut root = CPUOut{_ramForm = def, _nextRootNodeAddress = root}

{- | Phases of the FSM. The counter-carrying states (@ReadExternal@,
@WriteChange@, @WriteNewNodes@, @DelayWrite@) walk the `Interface` or the
fresh-node vector slot by slot.

Transitions:

- @Init@ → @FetchLeftActiveAddress@ (one cycle, output only the root address).
- @FetchLeftActiveAddress@ → @DelayFetch@ when an active address exists (its
  read is issued), → @Done@ when the net has no active pairs left.
- @DelayFetch@ → @FetchRightActiveAddress@ (re-drives the pending request).
- @FetchRightActiveAddress@ → @DelayReduce@: the left node has arrived; the
  right node's address is its principal port's target and is read now.
- @DelayReduce@ → @Reduce@ (re-drives the request).
- @Reduce@ → @ReadExternal 0@: the pair has been reduced, changes and
  interface computed.
- @ReadExternal counter@: reads the external node under the interface slot;
  → @DelayWrite counter@ once a non-empty address is found (its data returns
  two cycles later), → @WriteNewNodes 0@ past the end of the interface or
  when the interface is empty.
- @DelayWrite counter@ → @WriteChange counter@ (re-drives the read).
- @WriteChange counter@: writes the patched external node back;
  → @ReadExternal (counter + 1)@ or @WriteNewNodes 0@ at the last slot.
- @WriteNewNodes counter@: writes fresh nodes back; → @FetchLeftActiveAddress@
  at the last slot or when there are none (a reduction step always restarts
  the search — there is no edge to @Done@ here).
- @Done@: terminal for a finished net; stays there emitting the root address.
-}
data Phase (externalNodesNumber :: Nat) (newNodesNumber :: Nat)
  = Init
  | FetchLeftActiveAddress
  | FetchRightActiveAddress
  | Reduce
  | ReadExternal (Index externalNodesNumber)
  | WriteChange (Index externalNodesNumber)
  | WriteNewNodes (Index newNodesNumber)
  | Done
  | DelayReduce
  | DelayFetch
  | DelayWrite (Index externalNodesNumber)
  deriving (Generic, NFDataX, Show)

-- | Full FSM state. @externalNodesNumber@ is fixed to @2 * portsNumber@ (one interface slot per port of an active pair).
data CPUState portsNumber nodesNumber edgesNumber agentType = CPUState
  { _phase :: Phase ((*) 2 portsNumber) nodesNumber
  , _memoryManager :: MemoryManager
  , _previousLoadedNode :: Maybe (LoadedNode portsNumber agentType)
  -- ^ Left node of the pair, held while the right one is fetched
  , _previousRamForm :: Maybe (RamForm portsNumber agentType)
  -- ^ Pending request re-driven by the @Delay*@ phases
  , _changes :: Map ((*) 2 portsNumber) (Changes portsNumber)
  -- ^ Accumulated updates for the external nodes of the current pair
  , _interface :: Interface ((*) 2 portsNumber)
  -- ^ Addresses of the external nodes of the current pair
  , _nodesToWrite :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  -- ^ Fresh nodes of the current @Delta@
  , _rootNodeAddress :: AddressNumber
  }
  deriving (Generic, NFDataX)

instance (Show (CPUState portsNumber nodesNumber edgesNumber agentType)) where
  show CPUState{..} =
    "phase = "
      P.++ show _phase
      P.++ "\nchanges = "
      P.++ show _changes
      P.++ "\ninterface"
      P.++ show _interface

-- | Initial state: @Init@ phase, the given @MemoryManager@ and root address, everything else empty/`def`.
initCPUState ::
  (KnownNat portsNumber, KnownNat nodesNumber) =>
  MemoryManager ->
  AddressNumber ->
  CPUState portsNumber nodesNumber edgesNumber agentType
initCPUState initMM initRootNodeAddress =
  CPUState
    { _phase = Init
    , _memoryManager = initMM
    , _previousLoadedNode = def
    , _previousRamForm = def
    , _changes = def
    , _interface = def
    , _nodesToWrite = def
    , -- , _chooseReductionRule = reductionRule
      _rootNodeAddress = initRootNodeAddress
    }

{- | Advance the FSM by one cycle: consume the fetched node arriving on the
input, produce this cycle's `RamForm` transaction and the current root
address, and move to the next `Phase` (transitions documented there).

Per-phase behaviour:

- @Reduce@ is the only phase that reduces: `removeActivePair` frees the pair's
  cells, `reduce` applies the rule (allocation, rule body, rewiring,
  @writeNewActives@), @getInterface@ collects the external neighbours,
  `getAllChangesByDelta` precomputes their patches and `changeRootNode`
  carries the root across the reduction. A read of the left node's
  counterpart has been issued by then; its data is still in flight and is
  simply ignored by the counter phases.
- @ReadExternal@ issues a read-only transaction for the interface slot
  (@address, Just (address, Nothing)@), i.e. it never overwrites a cell.
- @WriteChange@ combines the node just fetched with the patch stored under
  its address (`find` in `_changes`) and writes the patched node back. A
  missing patch means the node is written back as `Nothing`, freeing the
  cell — by construction every interface node has at least one patch, so this
  only happens for a malformed @Delta@.
- @WriteNewNodes@ writes each fresh @LoadedNode@ at its `_originalAddress`
  and frees the slot by writing `Nothing` for absent entries.
- Address lookups in the interface or among new nodes use fixed-size vector
  indexing; the counters stop at the last slot, so no out-of-range access
  occurs for any parameter set.

Aborts (`errorX`) on: a fetched node without a principal port
(@FetchRightActiveAddress@), a vanished root (`changeRootNode`), or an
external node without changes (`updateLoadedNodesByChanges`' contract).
-}
step ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , Show agentType
  , Eq agentType
  ) =>
  -- CPUState portsNumber nodesNumber edgesNumber agentType ->
  CPUIn portsNumber agentType ->
  State (CPUState portsNumber nodesNumber edgesNumber agentType) (CPUOut portsNumber agentType)
step i@(CPUIn processedLoadedNode) =
  do
    s@(CPUState{..}) <- get
    case _phase of
      Init -> do
        put s{_phase = FetchLeftActiveAddress}
        pure $ defaultOut _rootNodeAddress
      FetchLeftActiveAddress ->
        if isJust ramForm
          then do
            put s{_phase = DelayFetch, _previousRamForm = ramForm}
            pure CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
          else do
            put s{_phase = Done}
            pure $ defaultOut _rootNodeAddress
       where
        activeAddress = giveActiveAddressNumber _memoryManager
        ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
      FetchRightActiveAddress -> do
        put s{_phase = DelayReduce, _previousLoadedNode = processedLoadedNode, _previousRamForm = ramForm}
        pure CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
       where
        activeAddress =
          (view nodeAddress . (fromMaybe (errorX "Wrong definition of active pair") <$> view primaryPort))
            . view containedNode
            <$> processedLoadedNode
        ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
      Reduce -> do
        put
          ( s
              { _phase = ReadExternal 0
              , _memoryManager = allocatedAddressesMemoryManager
              , _previousLoadedNode = def
              , _changes = allChanges
              , _interface = newInterface
              , _nodesToWrite = _newNodes delta
              , _rootNodeAddress = newRootAddress
              }
          )
        pure CPUOut{_ramForm = def, _nextRootNodeAddress = newRootAddress}
       where
        acPair = fromMaybe (errorX $ "cpu: 1. i = \n" P.++ show i) (ActivePair <$> _previousLoadedNode <*> processedLoadedNode)
        removedActivePairMemoryManager = removeActivePair acPair _memoryManager
        (delta, allocatedAddressesMemoryManager) = reduce @portsNumber @nodesNumber @edgesNumber @agentType removedActivePairMemoryManager acPair
        newInterface = getInterface @portsNumber @((*) 2 portsNumber) acPair
        allChanges = getAllChangesByDelta delta newInterface
        newRootAddress = changeRootNode acPair _rootNodeAddress delta
      ReadExternal counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = (,def) <$> _interface !! counter
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase
          | _ramForm == def && counter == (maxBound :: Index ((*) 2 portsNumber)) || _interface == def = WriteNewNodes 0
          | _ramForm == def = ReadExternal $ counter + 1
          | otherwise = DelayWrite counter
      WriteChange counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = do
          ln <- processedLoadedNode
          address <- _interface !! counter
          let newNode = applyChangesToNode (_containedNode ln) <$> find _changes address
           in pure (address, Just (address, newNode))
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase =
          if counter == (maxBound :: Index ((*) 2 portsNumber))
            then WriteNewNodes 0
            else ReadExternal $ counter + 1
      WriteNewNodes counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = (\(LoadedNode n a) -> (a, Just (a, Just n))) <$> _nodesToWrite !! counter
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase
          | counter == (maxBound :: Index nodesNumber) || (_nodesToWrite == def) = FetchLeftActiveAddress
          | otherwise = WriteNewNodes $ counter + 1
      Done -> pure $ defaultOut _rootNodeAddress
      DelayFetch -> do
        put s{_phase = FetchRightActiveAddress}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}
      DelayReduce -> do
        put s{_phase = Reduce}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}
      DelayWrite counter -> do
        put s{_phase = WriteChange counter}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}

-- | `step` as a synchronous circuit: @mealyS@ over the given initial @MemoryManager@ and root address.
mealyCore ::
  forall portsNumber nodesNumber edgesNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , Show agentType
  , NFDataX agentType
  , Eq agentType
  ) =>
  MemoryManager ->
  AddressNumber ->
  Signal dom (CPUIn portsNumber agentType) ->
  Signal dom (CPUOut portsNumber agentType)
mealyCore initialMM initRootNodeAddress =
  mealyS (step @portsNumber @nodesNumber @edgesNumber @agentType) (initCPUState initialMM initRootNodeAddress)
