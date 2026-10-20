-- | Cycle-cost tests pinning the static budget table of architecture.md
-- section 4.1 against the actual FSM behaviour.
--
-- The harness replicates the @Core.Core.logicBroad@ wiring (CPU FSM + @blockRam@
-- net + @t + 2@ RAM latency) but exposes the FSM 'Phase' every cycle, so each
-- 4.1 row becomes observable:
--
-- * startup: two leading @Init@ cycles (register power-on value plus the
--   one-cycle default reset re-holding it); the @Init@ transition itself
--   costs 1 cycle;
-- * @FetchLeft -> DelayFetch -> FetchRight -> DelayReduce -> Reduce@ = 5;
-- * interface walk: 3 per wired slot (@ReadExternal -> DelayWrite ->
--   WriteChange@), 1 per empty slot, 1 total when @\_interface == def@;
-- * @WriteNewNodes@ = 1 with no fresh nodes, = @nodesNumber@ otherwise;
-- * per-search window <= 19 for the topEntity parameters.
module Tests.Core.Unit.CycleBudget (cycleBudgetTests) where

import Clash.Prelude
import Control.Monad.State.Strict (runState)
import Core.CPU
import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda
import Core.Loader
import Core.MemoryManager.MemoryManager
import Core.Node
import Test.Tasty
import Test.Tasty.HUnit
import Prelude as P

-- | 'Core.Core.logicBroad' with the FSM phase exposed alongside the root
-- address, for the fixed @topEntity@ parameters (2 ports, 2 nodes, 2 edges).
traceBroad ::
  HiddenClockResetEnable System =>
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  AddressNumber ->
  MemoryManager ->
  Signal System (AddressNumber, Phase 4 2)
traceBroad initialNet initialRootNodeAddress initialMemoryManager =
  (\(ph, o) -> (_nextRootNodeAddress o, ph)) <$> cpu
 where
  ram = exposeEnable (blockRam initialNet)
  initialCPUIn = CPUIn def

  i = register initialCPUIn nextInput
  cpu = mealy tick (initCPUState initialMemoryManager initialRootNodeAddress) i
  out = snd <$> cpu

  tick ::
    CPUState 2 2 2 AgentSimpleLambda ->
    CPUIn 2 AgentSimpleLambda ->
    (CPUState 2 2 2 AgentSimpleLambda, (Phase 4 2, CPUOut 2 AgentSimpleLambda))
  tick state input = (state', (_phase state, o))
    where (o, state') = runState (step input) state

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

-- | Shown phase per sampled cycle: @trace net root mm n !! k@ is the phase the
-- FSM is executing in cycle @k@.
phases ::
  Int ->
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  AddressNumber ->
  MemoryManager ->
  [String]
phases n net root mm = P.map (show . snd) (trace net root mm n)

-- | Cycle indices where a new search starts (@FetchLeftActiveAddress@) plus the
-- first @Done@; consecutive deltas are the cycle budgets of the searches
-- (startup @Init@ cycles fall before the first mark).
searchWindows :: [String] -> [Int]
searchWindows ph = P.zipWith (-) (P.drop 1 marks) marks
 where
  flIdx = [i | (i, p) <- P.zip [0 :: Int ..] ph, p == "FetchLeftActiveAddress"]
  doneIdx = P.take 1 [i | (i, p) <- P.zip [0 :: Int ..] ph, p == "Done"]
  marks = flIdx P.++ doneIdx

betaReductionBudget :: TestTree
betaReductionBudget =
  testCase "beta: fetch 5, k=1 interface walk, no fresh nodes" $ do
    let ph = phases 30 initialIdApplyToIdNode 0 initialIdApplyToIdMM
        expected =
          [ "Init"
          , "Init" -- power-on + 1-cycle reset hold; Init transition = 1
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce" -- fetch chain = 5
          , "ReadExternal 0"
          , "DelayWrite 0"
          , "WriteChange 0" -- one wired slot = 3
          , "ReadExternal 1"
          , "ReadExternal 2"
          , "ReadExternal 3" -- three empty slots = 1 each
          , "WriteNewNodes 0" -- _nodesToWrite == def = 1
          , "FetchLeftActiveAddress" -- no active pair left
          ]
    assertEqual "phase sequence" expected (P.take 15 ph)
    assertBool "holds Done from cycle 15" (P.all (== "Done") (P.drop 15 ph))
    assertEqual
      "window = 5 + (3*1 + (4-1)) + 1 = 12; final search = 1"
      [12, 1]
      (searchWindows ph)
    assertBool "every window <= 19 (4.1 worst case)" (P.all (<= 19) (searchWindows ph))

erasePropagationBudget :: TestTree
erasePropagationBudget =
  testCase "erasure propagation: write-back = nodesNumber (2)" $ do
    let ph = phases 20 initialEpsAppToIdSimpleNode 1 initialEpsAppToIdSimpleMM
        expected =
          [ "Init"
          , "Init"
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          , "ReadExternal 0"
          , "DelayWrite 0"
          , "WriteChange 0"
          , "ReadExternal 1"
          , "ReadExternal 2"
          , "ReadExternal 3"
          , "WriteNewNodes 0"
          , "WriteNewNodes 1" -- fresh nodes: vector walked to maxBound = 2
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          ]
    assertEqual "phase sequence (20-cycle window)" expected ph
    assertEqual
      "window = 5 + (3*1 + (4-1)) + 2 = 13"
      [13]
      (searchWindows ph)
    assertBool "every window <= 19 (4.1 worst case)" (P.all (<= 19) (searchWindows ph))

multiReductionBudget :: TestTree
multiReductionBudget =
  testCase "four reductions: k=2/k=1/def walks and terminal search" $ do
    let ph = phases 50 initialEpsAppToIdNode 1 initialEpsAppToIdMM
        expected =
          [ "Init"
          , "Init"
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          , "ReadExternal 0"
          , "DelayWrite 0"
          , "WriteChange 0"
          , "ReadExternal 1"
          , "DelayWrite 1"
          , "WriteChange 1" -- k = 2 wired slots
          , "ReadExternal 2"
          , "ReadExternal 3"
          , "WriteNewNodes 0" -- no fresh nodes
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          , "ReadExternal 0"
          , "DelayWrite 0"
          , "WriteChange 0"
          , "ReadExternal 1"
          , "ReadExternal 2"
          , "ReadExternal 3" -- k = 1
          , "WriteNewNodes 0"
          , "WriteNewNodes 1"
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          , "ReadExternal 0" -- _interface == def: walk = 1
          , "WriteNewNodes 0"
          , "WriteNewNodes 1"
          , "FetchLeftActiveAddress"
          , "DelayFetch"
          , "FetchRightActiveAddress"
          , "DelayReduce"
          , "Reduce"
          , "ReadExternal 0" -- _interface == def: walk = 1
          , "WriteNewNodes 0" -- no fresh nodes
          , "FetchLeftActiveAddress"
          , "Done"
          ]
    assertEqual "phase sequence through termination" expected (P.take 46 ph)
    assertBool "holds Done from cycle 45" (P.all (== "Done") (P.drop 45 ph))
    assertEqual
      "windows: 5+(3*2+(4-2))+1 = 14, 5+6+2 = 13, 5+1+2 = 8, 5+1+1 = 7, final search = 1"
      [14, 13, 8, 7, 1]
      (searchWindows ph)
    assertBool "every window <= 19 (4.1 worst case)" (P.all (<= 19) (searchWindows ph))

cycleBudgetTests :: TestTree
cycleBudgetTests =
  testGroup
    "cycle budget (architecture.md section 4.1)"
    [ betaReductionBudget
    , erasePropagationBudget
    , multiReductionBudget
    ]
