{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.Core where

import Clash.Explicit.Prelude (unsafeSynchronizer)
import Clash.Prelude
import Control.Lens hiding ((:>))
import Core.Loader (Ram, loadActivePair, loadInterface, removeActivePairFromRam)
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager (
  MemoryManager,
  giveActiveAddressNumber,
  removeActivePair,
 )
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import INet.Net

data CoreStatus = Start | InProgress | Done deriving (Eq, Show, Generic, NFDataX, ShowX)

-- | Load program into ram
initializer ::
  forall portsNumber cellsNumber domFast agentType.
  ( KnownNat portsNumber
  , KnownDomain domFast
  , HiddenClockResetEnable domFast
  , NFDataX agentType
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  Ram domFast portsNumber agentType
initializer initialNetwork = myAsyncRam
 where
  myAsyncRam = blockRam initialNetwork
  readOut = undefined

{- | Full cycle of reductions.
It returns the root `Node` that
1) Contains exactly one `NotConnected` `Port`
1) Is the only one
3) Is contained in one and only component of connectivity
-}
core ::
  forall portsNumber nodesNumber edgesNumber cellsNumber domSlow domFast agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , KnownDomain domSlow
  , KnownDomain domFast
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  ) =>
  Clock domSlow ->
  Clock domFast ->
  Ram domFast portsNumber agentType ->
  AddressNumber ->
  MemoryManager cellsNumber -> -- Initial information about busy addresses and active pairs
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  (Signal domSlow AddressNumber, Signal domSlow CoreStatus) -- maybe we should return also memory manager
core clkSlow clkFast myAsyncRam initialRootNodeAddress initialMemoryManager chooseReductionRule = (rootNodeAddress, status)
 where
  -- registerExposed initial next= exposeClockResetEnable (register @domSlow initial next) clkSlow resetGen enableGen
  memoryManager = exposeClockResetEnable (register @domSlow initialMemoryManager nextMemoryManager) clkSlow resetGen enableGen
  rootNodeAddress = exposeClockResetEnable (register @domSlow initialRootNodeAddress nextRootNodeAddress) clkSlow resetGen enableGen
  status = exposeClockResetEnable (register @domSlow Start nextStatusCode) clkSlow resetGen enableGen

  (nextMemoryManager, nextRootNodeAddress, nextStatusCode) =
    case sequenceA $ giveActiveAddressNumber memoryManager of
      Just activeAddress ->
        ( allocatedAddressesMemoryManagerAsync
        , changeRootNode <$> acPairSlowed <*> rootNodeAddress <*> deltaAsync
        , pure InProgress
        )
       where
        -- read
        acPair = loadActivePair @domFast myAsyncRam (unsafeSynchronizer clkSlow clkFast activeAddress)
        acPairSlowed = unsafeSynchronizer clkFast clkSlow acPair
        -- ========
        -- no ram block
        -- ========
        -- first part of read
        removedActivePairMemoryManagerAsync = removeActivePair acPairSlowed memoryManager
        (deltaAsync, allocatedAddressesMemoryManagerAsync) = reduce chooseReductionRule removedActivePairMemoryManagerAsync acPairSlowed
        interfaceAsync = getInterface @portsNumber @nodesNumber <$> acPairSlowed

        -- read [Can I merge read and write?]
        externalNodesAsync = loadInterface myAsyncRam (unsafeSynchronizer clkSlow clkFast interfaceAsync)
        -- ======
        -- no ram block
        -- ======
        -- second part of read
        changesAsync =
          updateLoadedNodesByChanges
            <$> externalNodesAsync
            <*> unsafeSynchronizer clkSlow clkFast (getAllChangesByDelta deltaAsync interfaceAsync)

        -- write TODO: merge it
        !_ = removeActivePairFromRam myAsyncRam acPair
        !_ =
          writeChanges
            myAsyncRam
            changesAsync
      Nothing -> (memoryManager, rootNodeAddress, pure Done)

writeChanges ::
  (KnownNat maxNumOfChangedNodes, KnownNat portsNumber, KnownDomain domWrite) =>
  Ram domWrite portsNumber agentType ->
  Signal domWrite (Vec maxNumOfChangedNodes (Maybe (LoadedNode portsNumber agentType))) ->
  Vec maxNumOfChangedNodes (Signal domWrite (Maybe (Node portsNumber agentType)))
writeChanges ram changes = map writeByLoadedNode (unbundle changes)
 where
  writeByLoadedNode signalMaybeLoadedNode = case sequenceA signalMaybeLoadedNode of
    Nothing -> ram (pure 0) def
    Just signalLoadedNode ->
      let f = Just (view originalAddress <$> signalLoadedNode, Just . view containedNode <$> signalLoadedNode)
       in ram (pure 0) (traverse bundle f)
