{-# LANGUAGE AllowAmbiguousTypes #-}

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
