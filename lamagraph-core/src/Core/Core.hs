{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.Core where

import Clash.Prelude
import Control.Lens hiding ((:>))
import Core.Loader (Ram, loadActivePair, loadInterface)
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager (
  ActivePair,
  MemoryManager,
  giveActiveAddressNumber,
  leftNode,
  removeActivePair,
  rightNode,
 )
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer (ChooseReductionRule, getInterface, reducer)

core ::
  forall portsNumber nodesNumber edgesNumber cellsNumber dom.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , Enum AddressNumber
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber)) -> -- Initial network
  MemoryManager cellsNumber -> -- Initial information about busy addresses and active pairs
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber ->
  Signal dom (Vec cellsNumber (Maybe (Node portsNumber)))
core initialNetwork initialMemoryManager chooseReductionRule = bundle $ map (`ram` def) (unbundle allAddresses)
 where
  memoryManager = register @dom initialMemoryManager allocatedAddressesMemoryManager
  ram = blockRam initialNetwork :: Ram dom portsNumber
  activeAddress = case sequenceA $ giveActiveAddressNumber memoryManager of
    Just x -> x
    Nothing -> error "" -- end of program. TODO: add handling of this
  acPair = loadActivePair ram activeAddress
  removedActivePairMemoryManager = removeActivePair acPair memoryManager
  _ = removeActivePairFromRam ram acPair
  (delta, allocatedAddressesMemoryManager) = reducer chooseReductionRule removedActivePairMemoryManager acPair
  -- instead of "@portsNumber @nodesNumber" it possible to write ":: Signal dom (Interface nodesNumber)"
  interface = getInterface @portsNumber @nodesNumber <$> acPair
  externalNodes = loadInterface ram interface
  changes = updateLoadedNodesByChanges <$> externalNodes <*> getAllChangesByDelta delta interface
  _ = writeChanges ram changes
  allAddresses = pure (generateI (+ 1) (0 :: AddressNumber))

writeChanges ::
  (KnownNat maxNumOfChangedNodes, KnownNat portsNumber, KnownDomain dom, HiddenClockResetEnable dom) =>
  Ram dom portsNumber ->
  Signal dom (Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber))) ->
  Vec maxNumOfChangedNodes (Signal dom (Maybe (Node portsNumber)))
writeChanges ram changes = map writeByLoadedNode (unbundle changes)
 where
  writeByLoadedNode signalMaybeLoadedNode = case sequenceA signalMaybeLoadedNode of
    Nothing -> ram (pure 0) def
    Just signalLoadedNode ->
      let f = Just (view originalAddress <$> signalLoadedNode, Just . view containedNode <$> signalLoadedNode)
       in ram (pure 0) (traverse bundle f)

removeActivePairFromRam ::
  Ram dom portsNumber -> Signal dom (ActivePair portsNumber) -> Vec 2 (Signal dom (Maybe (Node portsNumber)))
removeActivePairFromRam ram acPair = map (\address -> partRam (Just <$> bundle (address, def))) (leftAddress :> rightAddress :> Nil)
 where
  partRam = ram def
  leftAddress = view (leftNode . originalAddress) <$> acPair
  rightAddress = view (rightNode . originalAddress) <$> acPair
