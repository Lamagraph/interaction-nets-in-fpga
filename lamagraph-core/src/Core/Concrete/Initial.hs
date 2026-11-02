{- |
Module      : Core.Concrete.Initial
Description : Hand-built initial nets and their @MemoryManager@s

Fixtures for the 3-agent lambda set: a net given as
`Vec CellsNumber (Maybe (Node 2 agentType))` (cells 0..n occupied, the rest
free) plus the matching @MemoryManager@ with the busy and active-pairs maps.
The nets are label-parametric where the synthesized and reference sets share
the same wiring: @topEntity@ builds `initialIdApplyToIdNodeWith` with the EDSL
labels of "INet.Define", while the rest of the fixtures use the hand-written
labels of "Core.Concrete.ReduceRulesLambda". @topEntity@ uses root address 0
(\"Core.Core\"; @docs/architecture.md\" describes the fixture).

Port wiring follows the mirror convention: a secondary port @Port a (Id j)@
always meets the mirror @Port a (Id (1 - j))@ on the other side, so the two
secondary ports of an agent appear swapped on the wire — self-loops of the
identity combinator (@\x. x@) are encoded as secondary 0 wired to secondary 1
and vice versa. Everything here is written by hand; a generator is planned
(the module is its own specification until then).
-}
module Core.Concrete.Initial where

import Clash.Prelude
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager (MemoryManager (MemoryManager), indexToUnsigned)
import Core.Node

-- | Tag every occupied cell with its own address, turning a raw net into @LoadedNode@s.
initialNodeToLoadedNode ::
  Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda)) ->
  Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialNodeToLoadedNode =
  imap
    ( \i maybeNode ->
        (\node -> LoadedNode node (indexToUnsigned i))
          <$> maybeNode
    )

{- | Initial Net view of \((\lambda x. x)(\lambda y. y)\), parameterized by the
runtime labels of the three-agent lambda set.

The net is identical for the hand-written set of
"Core.Concrete.ReduceRulesLambda" and the EDSL set of "INet.Define"; only the
label constructors differ, so the wiring is written once here and the labels
are supplied by the caller. The fixture has no `Erase` node, so the eraser
label is accepted for symmetry with the three-agent set but unused.
@topEntity@ uses the EDSL labels.

<<docs/apply.svg>>
-}
initialIdApplyToIdNodeWith ::
  agentType ->
  agentType ->
  agentType ->
  Vec CellsNumber (Maybe (Node 2 agentType))
initialIdApplyToIdNodeWith applyLabel abstractLabel _eraseLabel =
  Just applyNode +>> Just abstract1Node +>> Just abstract2Node +>> def
 where
  applyAddressNumber = 0
  abstract1AddressNumber = 1
  abstract2AddressNumber = 2
  applyNode =
    let prPort = Port abstract2AddressNumber Primary
        port1 = Port abstract1AddressNumber Primary
        secPorts = Just NotConnected :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts applyLabel
  abstract1Node =
    let prPort = Port applyAddressNumber (Id 1)
        port0 = Port abstract1AddressNumber (Id 1)
        port1 = Port abstract1AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts abstractLabel
  abstract2Node =
    let prPort = Port applyAddressNumber Primary
        port0 = Port abstract2AddressNumber (Id 1)
        port1 = Port abstract2AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts abstractLabel

-- | The fixture net with the hand-written set's labels.
initialIdApplyToIdNode :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
initialIdApplyToIdNode =
  initialIdApplyToIdNodeWith Apply Abstract Erase

-- | @LoadedNode@ view of `initialIdApplyToIdNode`.
initialIdApplyToId :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialIdApplyToId = initialNodeToLoadedNode initialIdApplyToIdNode

-- | @MemoryManager@ for `initialIdApplyToId`: cells 0-2 busy, one active pair.
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

-- | @MemoryManager@ for `initialLoopEdge`: cells 0-1 busy, one active pair.
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
        port0 = NotConnected
        port1 = Connected $ Port absAddressId Primary
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

-- | @LoadedNode@ view of `initialEpsAppToIdNode`.
initialEpsAppToId :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialEpsAppToId = initialNodeToLoadedNode initialEpsAppToIdNode

-- | @MemoryManager@ for `initialEpsAppToId`: cells 0-4 busy, active pairs at 1 and 3.
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

-- | @LoadedNode@ view of `initialEpsAppToIdSimpleNode`.
initialEpsAppToIdSimple :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialEpsAppToIdSimple = initialNodeToLoadedNode initialEpsAppToIdSimpleNode

-- | @MemoryManager@ for `initialEpsAppToIdSimple`: cells 0-2 busy, one active pair.
initialEpsAppToIdSimpleMM :: MemoryManager
initialEpsAppToIdSimpleMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @3) True ++ repeat False
  activePairsMap = (False :> True :> False :> Nil) ++ repeat False

-- | \(\lambda x. x\) connected to eraser by primary ports. It is not expressible in lambdas
initialIdErase :: Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda))
initialIdErase = initialNodeToLoadedNode initialIdEraseNode

-- | Raw net of `initialIdErase`.
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

-- | @MemoryManager@ for `initialIdErase`: cells 0-1 busy, one active pair.
initialIdEraseMM :: MemoryManager
initialIdEraseMM = MemoryManager busyMap activePairsMap
 where
  busyMap = replicate (SNat @2) True ++ repeat False
  activePairsMap = (True :> Nil) ++ repeat False
