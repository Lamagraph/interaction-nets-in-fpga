-- | Module with initial configuration. It will generate (by TemplateHaskell?) automatically in the future
module Core.Concrete.Initial where

import Clash.Prelude
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager (MemoryManager (MemoryManager))
import Core.Node

{- | Initial Net view of \((\lambda x. x)(\lambda y. y)\)

<<docs/apply.svg>>
-}
initialIdApplyToId :: Vec (2 ^ BitSize AddressNumber) (Maybe (LoadedNode 2 AgentSimpleLambda))
initialIdApplyToId = Just applyNode +>> Just abstract1Node +>> Just abstract2Node +>> def
 where
  applyAddressNumber = 0
  abstract1AddressNumber = 1
  abstract2AddressNumber = 2
  applyNode =
    let prPort = Port abstract2AddressNumber Primary
        port1 = Port abstract1AddressNumber Primary
        secPorts = Just NotConnected :> Just (Connected port1) :> Nil
     in LoadedNode (Node (Just prPort) secPorts Apply) applyAddressNumber
  abstract1Node =
    let prPort = Port applyAddressNumber (Id 1)
        port0 = Port abstract1AddressNumber (Id 1)
        port1 = Port abstract1AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in LoadedNode (Node (Just prPort) secPorts Abstract) abstract1AddressNumber
  abstract2Node =
    let prPort = Port applyAddressNumber Primary
        port0 = Port abstract2AddressNumber (Id 1)
        port1 = Port abstract2AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in LoadedNode (Node (Just prPort) secPorts Abstract) abstract2AddressNumber

initialIdApplyToIdMM :: MemoryManager (2 ^ BitSize AddressNumber)
initialIdApplyToIdMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @3) True ++ repeat False
  activePairsMap = replicate (SNat @1) True ++ repeat False

initialLoopEdge :: Vec (2 ^ BitSize AddressNumber) (Maybe (LoadedNode 2 AgentSimpleLambda))
initialLoopEdge = Just applyNode +>> Just abstractNode +>> def
 where
  applyAddressNumber = 0
  abstractAddressNumber = 1
  applyNode =
    let prPort = Connected $ Port abstractAddressNumber Primary
        port0 = Port applyAddressNumber (Id 1)
        port1 = Port applyAddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in LoadedNode (Node prPort secPorts Apply) applyAddressNumber
  abstractNode =
    let prPort = Connected $ Port applyAddressNumber Primary
        port0 = Port abstractAddressNumber (Id 1)
        port1 = Port abstractAddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in LoadedNode (Node prPort secPorts Abstract) abstractAddressNumber

initialLoopEdgeMM :: MemoryManager (2 ^ BitSize AddressNumber)
initialLoopEdgeMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @2) True ++ repeat False
  activePairsMap = replicate (SNat @1) True ++ repeat False
