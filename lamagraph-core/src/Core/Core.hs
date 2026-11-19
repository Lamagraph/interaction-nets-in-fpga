{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Core where

import Clash.Annotations.TH
import Clash.Prelude
import Core.CPU
import Core.Concrete.Initial (initialIdApplyToIdMM, initialIdApplyToIdNode)
import Core.Concrete.ReduceRulesLambda
import Core.Memory
import Core.MemoryManager.MemoryManager
import Core.Node
import INet.Net
import Protocols.Uart.GowinMaster

logicBoard ::
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
  , BitPack (Node portsNumber agentType)
  , BitPack agentType
  , 1 <= portsNumber
  ) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  AddressNumber ->
  MemoryManager ->
  (Signal dom AddressNumber, Signal dom Bit)
logicBoard initialNet initialRootNodeAddress initialMemoryManager = (_nextRootNodeAddress <$> o, tx)
 where
  o =
    mealyCore @portsNumber @nodesNumber @edgesNumber @agentType
      initialMemoryManager initialRootNodeAddress (CPUIn <$> memory)
  done = _done <$> o

  memory = blockRamMemory initialNet request
  request = mux done dumpRequest (toRamAddress . _toMem <$> o)
  (dumpRequest, nextLoadedNode) = dumpMemory done (fromEnable byteTransmitted) memory

  (tx, byteTransmitted) = transmitNodeByUart nextLoadedNode

  toRamAddress (ToMem r) = ToMem (convert <$> r)
  convert (MemRead address) = MemRead (fromIntegral address)
  convert (MemWrite address cell) = MemWrite (fromIntegral address) cell

createDomain vSystem{vName = "CustomDom", vPeriod = hzToPeriod 15e6, vResetPolarity = ActiveLow}

type PortsNumber = 2
type NodesNumber = 2
type EdgesNumber = 2
type AgentType = AgentSimpleLambda
type Dom = CustomDom

topEntity ::
  "clk" ::: Clock Dom ->
  "rstn" ::: Reset Dom ->
  -- "en" ::: Enable Dom ->
  ( "rootNodeAddress" ::: Signal Dom AddressNumber
  , "res_tx" ::: Signal Dom Bit
  )
topEntity clk rst = (rn, res_tx)
 where
  (rn, res_tx) =
    exposeClockResetEnable
      ( (logicBoard @PortsNumber @NodesNumber @EdgesNumber @AgentType)
          initialIdApplyToIdNode
          0
          initialIdApplyToIdMM
      )
      clk
      rst
      enableGen
{-# NOINLINE topEntity #-}
makeTopEntity 'topEntity
