{- | Simulation harness shared by the signal-level tests.

`netRam` loads a fixture net into a Clash @blockRam@ with its hidden enable
exposed, so the loader combinators can be threaded onto it;
`sampleSystemN` samples @n@ cycles of a @System@-domain signal after the
one-cycle reset every template uses.

`traceBroad` is the "Core.Core".@logicBroad@ wiring with the FSM 'Phase'
exposed next to the root address: the cycle-budget tests observe the phase of
every cycle. It is a deliberate copy of that wiring — @logicBroad@ hides the
phase — so it has to be kept in sync with "Core.Core" by hand.
-}
module Tests.Core.Unit.Common.Sim (
  netRam,
  sampleSystemN,
  traceBroad,
  trace,
  phases,
) where

import Clash.Prelude
import Control.Monad.State.Strict (runState)
import Core.CPU
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda)
import Core.Loader
import Core.MemoryManager.MemoryManager
import Core.Node
import Prelude as P

{- | The fixture net in a @blockRam@, with the hidden enable exposed.

Note the explicit lambda: writing this as the point-free
@exposeEnable . blockRam@ makes GHC resolve @blockRam@'s hidden clock
outside the simulation context, and the sampled RAM then never returns data.
-}
netRam ::
  (KnownNat portsNumber, NFDataX agentType, HiddenClockResetEnable System) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  Enable System ->
  Ram System portsNumber agentType
netRam net = exposeEnable (blockRam net)

-- | Sample @n@ cycles of a @System@-domain signal after a one-cycle reset.
sampleSystemN :: (NFDataX a) => Int -> ((HiddenClockResetEnable System) => Signal System a) -> [a]
sampleSystemN = sampleWithResetN @System d1

{- | "Core.Core".@logicBroad@ with the FSM phase exposed alongside the root
address, for the 2-node hand-written reference wiring (2 ports, 2 nodes,
2 edges). The synthesized `topEntity` uses the EDSL set with `nodesNumber = 4`;
the phase trace is kept on the reference wiring because the cycle-budget
tests pin its exact sequence.
-}
traceBroad ::
  (HiddenClockResetEnable System) =>
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  AddressNumber ->
  MemoryManager ->
  Signal System (AddressNumber, Phase 4 2)
traceBroad initialNet initialRootNodeAddress initialMemoryManager =
  (\(ph, o) -> (_nextRootNodeAddress o, ph)) <$> cpu
 where
  ram = netRam initialNet
  initialCPUIn = CPUIn def

  i = register initialCPUIn nextInput
  cpu = mealy tick (initCPUState initialMemoryManager initialRootNodeAddress) i
  out = snd <$> cpu

  tick ::
    CPUState 2 2 2 AgentSimpleLambda ->
    CPUIn 2 AgentSimpleLambda ->
    (CPUState 2 2 2 AgentSimpleLambda, (Phase 4 2, CPUOut 2 AgentSimpleLambda))
  tick state input = (state', (_phase state, o))
   where
    (o, state') = runState (step input) state

  nextLoadedNode = delayedMaybeRam ram (_ramForm <$> out)
  nextInput = CPUIn <$> (makeLoadedNodeFromRamForm <$> nextLoadedNode <*> (_ramForm <$> out))
  makeLoadedNodeFromRamForm n ramForm = LoadedNode <$> n <*> (fst <$> ramForm)

-- | Closed-loop sample of the traced wiring.
trace ::
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  AddressNumber ->
  MemoryManager ->
  Int ->
  [(AddressNumber, Phase 4 2)]
trace net root mm n = sampleN n (traceBroad net root mm)

{- | Shown phase per sampled cycle: @trace net root mm n !! k@ is the phase the
FSM is executing in cycle @k@.
-}
phases ::
  Int ->
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  AddressNumber ->
  MemoryManager ->
  [String]
phases n net root mm = P.map (show . snd) (trace net root mm n)
