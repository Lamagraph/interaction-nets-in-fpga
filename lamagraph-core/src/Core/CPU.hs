{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.CPU where

import Clash.Prelude
import Control.Lens hiding (Index)
import Control.Monad.State.Strict
import Core.Map
import Core.Memory
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe (fromMaybe)
import INet.Net
import qualified Prelude as P

newtype CPUIn portsNumber agentType = CPUIn (FromMem (Maybe (Node portsNumber agentType)))
  deriving (Generic, NFDataX, Show)

data CPUOut portsNumber agentType = CPUOut
  { _toMem :: ToMem AddressNumber (Maybe (Node portsNumber agentType))
  , _nextRootNodeAddress :: AddressNumber
  , _done :: Bool
  }
  deriving (Generic, ShowX, Eq, Show, NFDataX)

data Phase (externalNodesNumber :: Nat) (newNodesNumber :: Nat)
  = Init
  | FetchLeftActiveAddress
  | WaitLeft AddressNumber
  | FetchRightActiveAddress
  | WaitRight AddressNumber
  | ClearLeft
  | WaitClearLeft
  | ClearRight
  | WaitClearRight
  | Reduce
  | ReadExternal (Index externalNodesNumber)
  | WaitExternal (Index externalNodesNumber) AddressNumber
  | WriteChange (Index externalNodesNumber)
  | WaitChange (Index externalNodesNumber)
  | WriteNewNodes (Index newNodesNumber)
  | WaitNewNode (Index newNodesNumber)
  | Done
  deriving (Generic, NFDataX, Show, Eq)

data CPUState portsNumber nodesNumber edgesNumber agentType = CPUState
  { _phase :: Phase ((*) 2 portsNumber) nodesNumber
  , _memoryManager :: MemoryManager
  , _previousLoadedNode :: Maybe (LoadedNode portsNumber agentType)
  , _loadedNode :: Maybe (LoadedNode portsNumber agentType)
  , _changes :: Map ((*) 2 portsNumber) (Changes portsNumber)
  , _interface :: Interface ((*) 2 portsNumber)
  , _nodesToWrite :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  , _rootNodeAddress :: AddressNumber
  }
  deriving (Generic, NFDataX)

instance Show (CPUState portsNumber nodesNumber edgesNumber agentType) where
  show CPUState{..} =
    "phase = "
      P.++ show _phase
      P.++ "\nchanges = "
      P.++ show _changes
      P.++ "\ninterface"
      P.++ show _interface

initCPUState ::
  (KnownNat portsNumber, KnownNat nodesNumber) =>
  MemoryManager ->
  AddressNumber ->
  CPUState portsNumber nodesNumber edgesNumber agentType
initCPUState initMM initRootNodeAddress =
  CPUState
    { _phase = Init
    , _memoryManager = initMM
    , _previousLoadedNode = Nothing
    , _loadedNode = Nothing
    , _changes = def
    , _interface = def
    , _nodesToWrite = def
    , _rootNodeAddress = initRootNodeAddress
    }

step ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , Show agentType
  , Eq agentType
  ) =>
  CPUIn portsNumber agentType ->
  State (CPUState portsNumber nodesNumber edgesNumber agentType) (CPUOut portsNumber agentType)
step (CPUIn memory) = do
  s@CPUState{..} <- get
  let
    output request = CPUOut (ToMem request) _rootNodeAddress False
    advance phase = put s{_phase = phase} >> pure (output Nothing)
    issue request phase = do
      if ready memory then put s{_phase = phase} else pure ()
      pure (output (Just request))
    waitWrite phase = case resp memory of
      Just WriteAck -> advance phase
      _ -> pure (output Nothing)
    nextExternal counter
      | counter == maxBound = WriteNewNodes 0
      | otherwise = ReadExternal (counter + 1)
    nextNew counter
      | counter == maxBound = FetchLeftActiveAddress
      | otherwise = WriteNewNodes (counter + 1)
    left = fromMaybe (errorX "CPU: missing left active node") _previousLoadedNode
    right = fromMaybe (errorX "CPU: missing right active node") _loadedNode
  case _phase of
    Init -> advance FetchLeftActiveAddress
    FetchLeftActiveAddress -> case giveActiveAddressNumber _memoryManager of
      Nothing -> advance Done
      Just address -> issue (MemRead address) (WaitLeft address)
    WaitLeft address -> case resp memory of
      Just (ReadData cell) -> do
        put s{_phase = FetchRightActiveAddress, _previousLoadedNode = LoadedNode <$> cell <*> pure address}
        pure (output Nothing)
      _ -> pure (output Nothing)
    FetchRightActiveAddress ->
      let address = view nodeAddress $ fromMaybe (errorX "CPU: disconnected active pair") $ view (containedNode . primaryPort) left
       in issue (MemRead address) (WaitRight address)
    WaitRight address -> case resp memory of
      Just (ReadData cell) -> do
        put s{_phase = ClearLeft, _loadedNode = LoadedNode <$> cell <*> pure address}
        pure (output Nothing)
      _ -> pure (output Nothing)
    ClearLeft -> issue (MemWrite (view originalAddress left) Nothing) WaitClearLeft
    WaitClearLeft -> waitWrite ClearRight
    ClearRight -> issue (MemWrite (view originalAddress right) Nothing) WaitClearRight
    WaitClearRight -> waitWrite Reduce
    Reduce -> do
      let
        pair = ActivePair left right
        removed = removeActivePair pair _memoryManager
        (delta, allocated) = reduce @portsNumber @nodesNumber @edgesNumber @agentType removed pair
        newInterface = getInterface @portsNumber @((*) 2 portsNumber) pair
        newRoot = changeRootNode pair _rootNodeAddress delta
      put
        s
          { _phase = ReadExternal 0
          , _memoryManager = allocated
          , _previousLoadedNode = Nothing
          , _loadedNode = Nothing
          , _changes = getAllChangesByDelta delta newInterface
          , _interface = newInterface
          , _nodesToWrite = _newNodes delta
          , _rootNodeAddress = newRoot
          }
      pure (CPUOut (ToMem Nothing) newRoot False)
    ReadExternal counter -> case _interface !! counter of
      Nothing -> advance (nextExternal counter)
      Just address -> issue (MemRead address) (WaitExternal counter address)
    WaitExternal counter address -> case resp memory of
      Just (ReadData cell) -> do
        put s{_phase = WriteChange counter, _loadedNode = LoadedNode <$> cell <*> pure address}
        pure (output Nothing)
      _ -> pure (output Nothing)
    WriteChange counter -> case (_interface !! counter, _loadedNode) of
      (Just address, Just loaded) ->
        let cell = applyChangesToNode (view containedNode loaded) <$> find _changes address
         in issue (MemWrite address cell) (WaitChange counter)
      _ -> advance (nextExternal counter)
    WaitChange counter -> waitWrite (nextExternal counter)
    WriteNewNodes counter -> case _nodesToWrite !! counter of
      Nothing -> advance (nextNew counter)
      Just (LoadedNode node address) -> issue (MemWrite address (Just node)) (WaitNewNode counter)
    WaitNewNode counter -> waitWrite (nextNew counter)
    Done -> pure (CPUOut (ToMem Nothing) _rootNodeAddress True)

mealyCore ::
  forall portsNumber nodesNumber edgesNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , Show agentType
  , NFDataX agentType
  , Eq agentType
  ) =>
  MemoryManager ->
  AddressNumber ->
  Signal dom (CPUIn portsNumber agentType) ->
  Signal dom (CPUOut portsNumber agentType)
mealyCore initialMM initRootNodeAddress =
  mealyS (step @portsNumber @nodesNumber @edgesNumber @agentType) (initCPUState initialMM initRootNodeAddress)
