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
delayedMaybeRam ram inputAddress =
  mux
    (isJust <$> delayedAddress)
    (exposedRam $ unbundle $ fromJustX <$> inputAddress)
    def
 where
  delayedAddress = delay Nothing inputAddress
  enableFlag = toEnable $ isJust <$> inputAddress .&&. (delayedAddress ./=. inputAddress)
  exposedRam (readAddress, writeData) = ram enableFlag readAddress writeData

getRamFormInterface ::
  ( KnownNat portsNumber
  , KnownNat externalNodesNumber
  ) =>
  Interface externalNodesNumber ->
  Vec externalNodesNumber (Maybe (RamForm portsNumber agentType))
getRamFormInterface = map (fmap (,def))

-- | Read external `Node`s from ram
loadInterface ::
  forall dom portsNumber externalNodesNumber agentType.
  ( KnownDomain dom
  , KnownNat portsNumber
  , KnownNat externalNodesNumber
  , NFDataX agentType
  , HiddenClockResetEnable dom
  , Eq agentType
  ) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Interface externalNodesNumber) ->
  Signal dom (Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)))
loadInterface ram interface =
  bundle $
    map
      (\ramForm -> fromRamFormToLoadedNode <$> delayedMaybeRam ram ramForm <*> ramForm)
      (unbundle interfaceRamForm)
 where
  interfaceRamForm = getRamFormInterface <$> interface
  fromRamFormToLoadedNode maybeNode maybeRamForm = let address = fst <$> maybeRamForm in LoadedNode <$> maybeNode <*> address

-- | Load `ActivePair` by `AddressNumber`. It is assumed that `AddressNumber` is actually active
loadActivePair ::
  (KnownDomain dom, KnownNat portsNumber, HiddenClockResetEnable dom, NFDataX agentType, Eq agentType) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Maybe AddressNumber) ->
  Signal dom (Maybe (ActivePair portsNumber agentType))
loadActivePair ram leftActiveNodeAddress = toMaybeActivePair <$> maybeLeftLoadedNode <*> maybeRightLoadedNode
 where
  getRightNodeAddress maybeLoadedNode =
    (view nodeAddress . (fromMaybe (errorX "Wrong definition of active pair") <$> view primaryPort)) . view containedNode
      <$> maybeLoadedNode
  addressToRamForm a = (\address -> (address, Just (address, Nothing))) <$> a
  fromRamFormToLoadedNode maybeNode maybeAddress = LoadedNode <$> maybeNode <*> maybeAddress
  constructLoadedNode address = fromRamFormToLoadedNode <$> delayedMaybeRam ram (addressToRamForm <$> address) <*> address
  maybeLeftLoadedNode = constructLoadedNode leftActiveNodeAddress
  maybeRightLoadedNode = constructLoadedNode $ getRightNodeAddress <$> maybeLeftLoadedNode
  toMaybeActivePair leftLoadedNode rightLoadedNode = ActivePair <$> leftLoadedNode <*> rightLoadedNode

writeChanges ::
  ( KnownNat maxNumOfChangedNodes
  , KnownNat portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , NFDataX agentType
  , Eq agentType
  ) =>
  (Enable dom -> Ram dom portsNumber agentType) ->
  Signal dom (Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber agentType))) ->
  Vec maxNumOfChangedNodes (Signal dom (Maybe (Node portsNumber agentType)))
writeChanges ram changes = map (\x -> delayedMaybeRam ram (maybeLoadedNodeToRamForm <$> x)) (unbundle changes)
 where
  maybeLoadedNodeToRamForm maybeLn = (\LoadedNode{..} -> (_originalAddress, Just (_originalAddress, Just _containedNode))) <$> maybeLn
