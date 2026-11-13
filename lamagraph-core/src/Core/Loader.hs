{- |
Module      : Core.Loader
Description : Signal-level access to the net RAM

Moves nodes between the net RAM and the CPU as `Signal`s: fetch a node by
address, write updated nodes back. The CPU FSM in "Core.CPU" inlines this
logic phase by phase; the standalone combinators below remain for tests and
as a reference implementation, and the three fetch/write helpers are
deprecated in favor of that inline logic.

All reads are synchronous (one RAM cycle) plus, where noted, an explicit
@delay@, so data for an address issued in cycle @t@ is usable at @t + 2@ —
the latency the CPU's @Delay*@ phases account for.
-}
module Core.Loader where

import Clash.Prelude
import Control.Lens (view)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Data.Maybe (fromMaybe, isJust)

-- | Type alias for partial applied `blockRam`
type Ram dom portsNumber agentType =
  ( Signal dom AddressNumber ->
    Signal dom (Maybe (AddressNumber, Maybe (Node portsNumber agentType))) ->
    Signal dom (Maybe (Node portsNumber agentType))
  )

{- | A RAM transaction: the address to read, and — in the second component —
@Nothing@ for \"no write\", or @Just (address, maybeNode)@ where `Nothing`
node frees the cell and `Just` writes the node there.
-}
type RamForm portsNumber agentType = (AddressNumber, Maybe (AddressNumber, Maybe (Node portsNumber agentType)))

{- | Read and Write by `Maybe` `RamForm` from RAM. It returns `Nothing` if the input
is `Nothing` and __does not touch ram__ in that cycle.

Contract: the value for a `RamForm` issued in cycle @t@ appears at the output
in cycle @t + 2@ (one cycle of synchronous RAM plus the internal @delay@ of the
request); a write issued in cycle @t@ is committed by the end of cycle @t + 1@.
-}
delayedMaybeRam ::
  (KnownDomain dom, HiddenClockResetEnable dom, KnownNat portsNumber, NFDataX agentType, Eq agentType) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Maybe (RamForm portsNumber agentType)) ->
  Signal dom (Maybe (Node portsNumber agentType))
delayedMaybeRam ram inputRamForm =
  mux
    (isJust <$> delayedRamForm)
    (exposedRam $ unbundle $ fromJustX <$> inputRamForm)
    def
 where
  delayedRamForm = delay Nothing inputRamForm
  enableFlag = toEnable $ isJust <$> inputRamForm
  exposedRam (readAddress, writeData) = ram enableFlag readAddress writeData

{- | Read and Write by `Maybe` `RamForm` from RAM. It return `Nothing` if input is `Nothing` and __do not touch ram__
Specialized version with an inequality condition for the values of the current and previous cycles
__Deprecated__
-}
delayedMaybeRamWithNEQ ::
  (KnownDomain dom, HiddenClockResetEnable dom, KnownNat portsNumber, NFDataX agentType, Eq agentType) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Maybe (RamForm portsNumber agentType)) ->
  Signal dom (Maybe (Node portsNumber agentType))
delayedMaybeRamWithNEQ ram inputRamForm =
  mux
    (isJust <$> delayedRamForm)
    (exposedRam $ unbundle $ fromJustX <$> inputRamForm)
    def
 where
  delayedRamForm = delay Nothing inputRamForm
  enableFlag = toEnable $ isJust <$> inputRamForm .&&. (delayedRamForm ./=. inputRamForm)
  exposedRam (readAddress, writeData) = ram enableFlag readAddress writeData

{- | Read external @Node@s from ram.

Fetches one @LoadedNode@ per `Interface` slot by issuing each address as a
`RamForm` that writes nothing (@Just (address, Nothing)@); @changedInterface@
supplies per-slot updates already written back. Unused slots stay `Nothing`.

Deprecated: the CPU FSM performs the same fetches inline in its
@ReadExternal@ \/ @WriteChange@ phases of "Core.CPU".
-}
{-# DEPRECATED loadInterface "use inline logic in Core.CPU" #-}
loadInterface ::
  forall dom portsNumber externalNodesNumber agentType.
  ( KnownDomain dom
  , KnownNat portsNumber
  , KnownNat externalNodesNumber
  , NFDataX agentType
  , HiddenClockResetEnable dom
  , Eq agentType
  , Show agentType
  ) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Interface externalNodesNumber) ->
  Signal dom (Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType))) ->
  Signal dom (Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)))
loadInterface ram interface changedInterface =
  bundle $
    map
      (\ramForm -> fromRamFormToLoadedNode <$> delayedMaybeRam ram ramForm <*> ramForm)
      (unbundle interfaceRamForm)
 where
  interfaceRamForm = zipInterfaceUpdatedToRamForm <$> interface <*> changedInterface
  fromRamFormToLoadedNode maybeNode maybeRamForm = let address = fst <$> maybeRamForm in LoadedNode <$> maybeNode <*> address

{- | Load @ActivePair@ by `AddressNumber`. It is assumed that `AddressNumber` is actually active.

Reads the left node, follows its principal port to get the right address and
reads the right node; both fetches go through `delayedMaybeRamWithNEQ`.

Deprecated: the CPU FSM performs the same fetch inline in its
@FetchLeftActiveAddress@ \/ @DelayFetch@ \/ @FetchRightActiveAddress@ phases
of "Core.CPU". Note the test suite still exercises this function.
-}
{-# DEPRECATED loadActivePair "use inline logic in Core.CPU" #-}
loadActivePair ::
  (KnownDomain dom, KnownNat portsNumber, HiddenClockResetEnable dom, NFDataX agentType, Eq agentType) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Maybe AddressNumber) ->
  Signal dom (Maybe (ActivePair portsNumber agentType))
loadActivePair ram leftActiveNodeAddress = toMaybeActivePair <$> maybeLeftLoadedNode <*> maybeRightLoadedNode
 where
  getRightNodeAddress maybeLoadedNode =
    (view nodeAddress . (fromMaybe (errorX "Wrong definition of active pair") <$> view primaryPort))
      . view containedNode
      <$> maybeLoadedNode
  addressToRamForm a = (\address -> (address, Just (address, Nothing))) <$> a
  fromRamFormToLoadedNode maybeNode maybeAddress = LoadedNode <$> maybeNode <*> maybeAddress
  constructLoadedNode address = fromRamFormToLoadedNode <$> delayedMaybeRamWithNEQ ram (addressToRamForm <$> address) <*> address
  maybeLeftLoadedNode = constructLoadedNode leftActiveNodeAddress
  maybeRightLoadedNode = constructLoadedNode $ getRightNodeAddress <$> maybeLeftLoadedNode
  toMaybeActivePair leftLoadedNode rightLoadedNode = ActivePair <$> leftLoadedNode <*> rightLoadedNode

{- | Write new @Node@s to RAM: one write per non-`Nothing` slot, each at the
node's `_originalAddress`.

Deprecated: the CPU FSM performs the same writes inline in its
@WriteNewNodes@ phase of "Core.CPU".
-}
{-# DEPRECATED writeNewNodes "use inline logic in Core.CPU" #-}
writeNewNodes ::
  ( KnownNat nodesNumber
  , KnownNat portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , NFDataX agentType
  , Eq agentType
  ) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))) ->
  Vec nodesNumber (Signal dom (Maybe (Node portsNumber agentType)))
writeNewNodes ram changes = map (\x -> delayedMaybeRam ram (maybeLoadedNodeToRamForm <$> x)) (unbundle changes)
 where
  maybeLoadedNodeToRamForm maybeLn = (\LoadedNode{..} -> (_originalAddress, Just (_originalAddress, Just _containedNode))) <$> maybeLn

{- | Pair every `Interface` slot with the `RamForm` that rewrites its node back
to RAM (a write-only transaction: read address = write address).

Aborts with `errorX` on inconsistent input: a loaded node whose
`_originalAddress` differs from its interface address, or a `Nothing` interface
slot paired with a present node. A present interface slot with `Nothing` node
writes nothing (the slot read as empty).
-}
zipInterfaceUpdatedToRamForm ::
  (Show agentType) =>
  Interface externalNodesNumber ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  Vec externalNodesNumber (Maybe (RamForm portsNumber agentType))
zipInterfaceUpdatedToRamForm =
  zipWith
    ( \maybeAddress maybeLoadedNode -> case (maybeAddress, maybeLoadedNode) of
        (Just address, Just (LoadedNode{..})) ->
          if _originalAddress == address
            then Just (address, Just (_originalAddress, Just _containedNode))
            else errorX "interfaced address and address of loaded node are not equal"
        (Nothing, Nothing) -> def
        (Just address, Nothing) -> Just (address, def)
        (Nothing, Just _) -> errorX " interfaced address is Nothing and external node is not "
    )
