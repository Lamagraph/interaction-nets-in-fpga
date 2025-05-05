-- | Module with initial configuration. It will generate (by TemplateHaskell?) automatically in the future
module Core.Concrete.Initial where

import Clash.Prelude
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager (MemoryManager (MemoryManager), indexToUnsigned)
import Core.Node

initialNodeToLoadedNode ::
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialNodeToLoadedNode =
  imap
    ( \i maybeNode ->
        (\node -> LoadedNode node (indexToUnsigned i))
          <$> maybeNode
    )

{- | Initial Net view of \((\lambda x. x)(\lambda y. y)\)

<<docs/apply.svg>>
-}
initialIdApplyToIdNode :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
initialIdApplyToIdNode = Just applyNode +>> Just abstract1Node +>> Just abstract2Node +>> def
 where
  applyAddressNumber = 0
  abstract1AddressNumber = 1
  abstract2AddressNumber = 2
  applyNode =
    let prPort = Port abstract2AddressNumber Primary
        port1 = Port abstract1AddressNumber Primary
        secPorts = Just NotConnected :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Apply
  abstract1Node =
    let prPort = Port applyAddressNumber (Id 1)
        port0 = Port abstract1AddressNumber (Id 1)
        port1 = Port abstract1AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Abstract
  abstract2Node =
    let prPort = Port applyAddressNumber Primary
        port0 = Port abstract2AddressNumber (Id 1)
        port1 = Port abstract2AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Abstract

initialIdApplyToId :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialIdApplyToId = initialNodeToLoadedNode initialIdApplyToIdNode

initialIdApplyToIdMM :: MemoryManager
initialIdApplyToIdMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @3) True ++ repeat False
  activePairsMap = replicate (SNat @1) True ++ repeat False

-- | Not correct net that should reduce to `Nothing`
initialLoopEdge :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
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

initialLoopEdgeMM :: MemoryManager
initialLoopEdgeMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @2) True ++ repeat False
  activePairsMap = replicate (SNat @1) True ++ repeat False

-- | \((\lambda x. (\varepsilon) (x))(\lambda y. y)\)
initialEpsAppToIdNode :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
initialEpsAppToIdNode = Just absIdNode +>> Just applyMainNode +>> Just absEraseNode +>> Just applyEraseNode +>> Just eraseNode +>> def
 where
  absAddressId = 0
  applyAddressMain = 1
  absAddressErase = 2
  applyAddressErase = 3
  eraseAddress = 4
  absIdNode =
    let prPort = Connected $ Port applyAddressMain (Id 1)
        port0 = Connected $ Port absAddressId (Id 1)
        port1 = Connected $ Port absAddressId (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Abstract
  applyMainNode =
    let prPort = Connected $ Port absAddressErase Primary
        port0 = Connected $ Port absAddressId Primary
        port1 = NotConnected
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Apply
  absEraseNode =
    let prPort = Connected $ Port applyAddressMain Primary
        port0 = Connected $ Port applyAddressErase (Id 1)
        port1 = Connected $ Port applyAddressErase (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Abstract
  applyEraseNode =
    let prPort = Connected $ Port eraseAddress Primary
        port0 = Connected $ Port absAddressErase (Id 1)
        port1 = Connected $ Port absAddressErase (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Apply
  eraseNode =
    let prPort = Connected $ Port applyAddressErase Primary
        secPorts = Nothing :> Nothing :> Nil
     in Node prPort secPorts Erase

initialEpsAppToId :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialEpsAppToId = initialNodeToLoadedNode initialEpsAppToIdNode

initialEpsAppToIdMM :: MemoryManager
initialEpsAppToIdMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @5) True ++ repeat False
  activePairsMap = (False :> True :> False :> True :> Nil) ++ repeat False

-- | \(( \varepsilon (\lambda y. y)\)
initialEpsAppToIdSimpleNode :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
initialEpsAppToIdSimpleNode = Just absIdNode +>> Just applyEraseNode +>> Just eraseNode +>> def
 where
  absAddressId = 0
  applyAddressErase = 1
  eraseAddress = 2
  absIdNode =
    let prPort = Connected $ Port applyAddressErase (Id 1)
        port0 = Connected $ Port absAddressId (Id 1)
        port1 = Connected $ Port absAddressId (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Abstract
  applyEraseNode =
    let prPort = Connected $ Port eraseAddress Primary
        port0 = NotConnected
        port1 = Connected $ Port absAddressId Primary
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Apply
  eraseNode =
    let prPort = Connected $ Port applyAddressErase Primary
        secPorts = Nothing :> Nothing :> Nil
     in Node prPort secPorts Erase

initialEpsAppToIdSimple :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialEpsAppToIdSimple = initialNodeToLoadedNode initialEpsAppToIdSimpleNode

initialEpsAppToIdSimpleMM :: MemoryManager
initialEpsAppToIdSimpleMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @3) True ++ repeat False
  activePairsMap = (False :> True :> False :> Nil) ++ repeat False

-- | \(\lambda x. x\) connected to eraser by primary ports. It is not expressible in lambdas
initialIdErase :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialIdErase = initialNodeToLoadedNode initialIdEraseNode

initialIdEraseNode :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
initialIdEraseNode = Just idNode +>> Just eraseNode +>> def
 where
  idAddress = 0
  eraseAddress = 1
  idNode =
    let prPort = Connected $ Port eraseAddress Primary
        port0 = Connected $ Port idAddress (Id 1)
        port1 = Connected $ Port idAddress (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in Node prPort secPorts Abstract
  eraseNode =
    let prPort = Connected $ Port idAddress Primary
        secPorts = def
     in Node prPort secPorts Erase

initialIdEraseMM :: MemoryManager
initialIdEraseMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @2) True ++ repeat False
  activePairsMap = (True :> Nil) ++ repeat False
