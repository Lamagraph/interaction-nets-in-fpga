{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Core.Core
Description : Top-level wiring of CPU and RAM; the `topEntity` fixture

Closes the hardware loop: `logicBroad` instantiates the CPU FSM ("Core.CPU")
together with a @blockRam@ holding the net, and feeds each cycle's RAM read
data back as the next cycle's CPU input. @topEntity@ fixes all parameters —
2 secondary ports, 2 nodes, 2 edges, `AgentSimpleLambda` from
"Core.Concrete.ReduceRulesLambda" — the initial net
(`initialIdApplyToIdNode`, the fixture \((\lambda x. x)(\lambda y. y)\)),
its @MemoryManager@ and root node address 0.

Timing: the RAM answers in one cycle and the CPU input is registered, so the
node fetched for an address issued in cycle @t@ is processed in cycle @t + 2@.

The repository's @docs/architecture.md@ describes the pipeline and the CPU
FSM this wiring implements; @docs/build.md@ covers HDL generation.
-}
module Core.Core where

import Clash.Annotations.TH
import Clash.Prelude
import Core.CPU
import Core.Concrete.Initial (initialIdApplyToIdMM, initialIdApplyToIdNode)
import Core.Concrete.ReduceRulesLambda
import Core.Loader
import Core.MemoryManager.MemoryManager
import Core.Node
import INet.Net

{- | One reduction processor: CPU FSM plus the RAM with the net.

Each cycle the CPU emits a `RamForm` (read address plus optional write
payload). The read half is answered by the RAM one cycle later and arrives at
the CPU two cycles after the request (one cycle RAM latency plus the input
register @i@); `logicBroad` compensates for that with the @Delay*@ phases of
the FSM. The output is the address of the current root node — the result of
the whole computation once the FSM reaches `Done`.
-}
logicBroad ::
  forall portsNumber nodesNumber edgesNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownDomain dom
  , INet agentType nodesNumber edgesNumber portsNumber
  , nodesNumber <= CellsNumber
  , HiddenClockResetEnable dom
  , NFDataX agentType
  , Show agentType
  , Eq agentType
  ) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  AddressNumber ->
  MemoryManager ->
  Signal dom AddressNumber
logicBroad initialNet initialRootNodeAddress initialMemoryManager = _nextRootNodeAddress <$> o
 where
  ram = exposeEnable (blockRam initialNet)
  initialCPUIn = CPUIn def

  i = register initialCPUIn nextInput
  o =
    mealyCore @portsNumber @nodesNumber @edgesNumber @agentType initialMemoryManager initialRootNodeAddress i

  nextLoadedNode = delayedMaybeRam ram (_ramForm <$> o)

  nextInput = CPUIn <$> (makeLoadedNodeFromRamForm <$> nextLoadedNode <*> (_ramForm <$> o))

  makeLoadedNodeFromRamForm n ramForm = LoadedNode <$> n <*> (fst <$> ramForm)

{- | Synthesizable top level: net \((\lambda x. x)(\lambda y. y)\) on the
2-port @AgentSimpleLambda@ rule set. Returns the root node address stream
(fixed at 0 during the initial cycles of the FSM).
-}
topEntity ::
  "clk" ::: Clock System ->
  "rst" ::: Reset System ->
  "en" ::: Enable System ->
  "rootNodeAddress" ::: Signal System AddressNumber
topEntity =
  exposeClockResetEnable
    ( (logicBroad @2 @2 @2 @AgentSimpleLambda)
        initialIdApplyToIdNode
        0
        initialIdApplyToIdMM
    )
{-# NOINLINE topEntity #-}
makeTopEntity 'topEntity
