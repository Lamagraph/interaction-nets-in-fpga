{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.Core where

import Clash.Explicit.Prelude (unsafeSynchronizer)
import Clash.Prelude
import Core.Loader (Ram, loadActivePair, loadInterface, removeActivePairFromRam, writeChanges)
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager (
  MemoryManager,
  giveActiveAddressNumber,
  removeActivePair,
 )
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe (fromJust, isJust)
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
  memoryManager = exposeClockResetEnable (register @domSlow initialMemoryManager nextMemoryManager) clkSlow resetGen enableGen
  rootNodeAddress = exposeClockResetEnable (register @domSlow initialRootNodeAddress nextRootNodeAddress) clkSlow resetGen enableGen
  status = exposeClockResetEnable (register @domSlow Start nextStatusCode) clkSlow resetGen enableGen

  (nextMemoryManager, nextRootNodeAddress, nextStatusCode) =
    unbundle $
      mux
        thereIsActivePairExists
        (bundle $ takeAStep (fromJust <$> giveActiveAddressNumber memoryManager))
        (bundle (memoryManager, rootNodeAddress, pure Done))
   where
    thereIsActivePairExists = isJust <$> giveActiveAddressNumber memoryManager
    takeAStep activeAddress =
      ( allocatedAddressesMemoryManager
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
      removedActivePairMemoryManager = removeActivePair acPairSlowed memoryManager
      (deltaAsync, allocatedAddressesMemoryManager) = reduce chooseReductionRule removedActivePairMemoryManager acPairSlowed
      interface = getInterface @portsNumber @((*) 2 portsNumber) <$> acPairSlowed

      -- read [Can I merge read and write?]
      externalNodes = loadInterface myAsyncRam (unsafeSynchronizer clkSlow clkFast interface)
      -- ======
      -- no ram block
      -- ======
      -- second part of read
      changes =
        updateLoadedNodesByChanges
          <$> externalNodes
          <*> unsafeSynchronizer clkSlow clkFast (getAllChangesByDelta deltaAsync interface)

      -- write TODO: merge it
      !_ = removeActivePairFromRam myAsyncRam acPair
      !_ =
        writeChanges
          myAsyncRam
          changes
