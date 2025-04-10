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

type RamForm portsNumber agentType = (AddressNumber, Maybe (AddressNumber, Maybe (Node portsNumber agentType)))

-- | Read and Write by `Maybe` `RamForm` from RAM. It return `Nothing` if input is `Nothing` and __do not touch ram__
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

-- | Read external `Node`s from ram
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

{- | Load `ActivePair` by `AddressNumber`. It is assumed that `AddressNumber` is actually active

__Deprecated__
-}
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

{- | Write new `Node`s to RAM

__Deprecated__
-}
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
