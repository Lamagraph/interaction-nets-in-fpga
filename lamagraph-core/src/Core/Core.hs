module Core.Core where

import Clash.Prelude
import Core.CPU
import Core.Loader
import Core.MemoryManager.MemoryManager
import Core.Node
import INet.Net

logicBroad ::
  forall portsNumber nodesNumber edgesNumber cellsNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownDomain dom
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , HiddenClockResetEnable dom
  , NFDataX agentType
  , Show agentType
  , Eq agentType
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  AddressNumber ->
  MemoryManager cellsNumber -> -- Initial information about busy addresses and active pairs
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  Signal dom AddressNumber
logicBroad initialNet initialRootNodeAddress initialMemoryManager chooseReductionRule = _nextRootNodeAddress <$> o
 where
  ram = exposeEnable (blockRam initialNet)
  initialCPUIn = CPUIn def

  i = register initialCPUIn nextInput
  o = mealyCore initialMemoryManager chooseReductionRule initialRootNodeAddress i

  nextLoadedNode = delayedMaybeRam ram (_ramForm <$> o)

  nextInput = CPUIn <$> (makeLoadedNodeFromRamForm <$> nextLoadedNode <*> (_ramForm <$> o))

  makeLoadedNodeFromRamForm n ramForm = LoadedNode <$> n <*> (fst <$> ramForm)
