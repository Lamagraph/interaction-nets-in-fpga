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
import Core.Loader
import Core.Map
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe (fromMaybe, isJust)
import INet.Net (INet, ReduceRuleResult, getAddressesAllocationCount, makeReduction)
import qualified Prelude as P

newtype CPUIn portsNumber agentType = CPUIn (Maybe (LoadedNode portsNumber agentType))
  deriving (Generic, NFDataX, Show)

data CPUOut portsNumber agentType = CPUOut
  { _ramForm :: Maybe (RamForm portsNumber agentType)
  , _nextRootNodeAddress :: AddressNumber
  }
  deriving (Generic, ShowX, Eq, Show, NFDataX)

defaultOut :: AddressNumber -> CPUOut portsNumber agentType
defaultOut root = CPUOut{_ramForm = def, _nextRootNodeAddress = root}

data Phase (externalNodesNumber :: Nat) (newNodesNumber :: Nat)
  = Init
  | FetchLeftActiveAddress
  | FetchRightActiveAddress
  | {- | Apply the reduction rule to the active pair; store the
    ReduceRuleResult and the allocated MemoryManager.
    -}
    ReduceRule
  | -- | Build the Delta from the stored ReduceRuleResult.
    ReduceDelta
  | -- | Compute the Interface and Changes from the stored Delta.
    ReduceInterface
  | {- | Commit: update the root address and MemoryManager; advance to
    ReadExternal 0 (or WriteNewNodes 0 if the interface is empty).
    This is the only frame that carries the new root address.
    -}
    ReduceCommit
  | ReadExternal (Index externalNodesNumber)
  | WriteChange (Index externalNodesNumber)
  | WriteNewNodes (Index newNodesNumber)
  | Done
  | DelayReduce
  | DelayFetch
  | DelayWrite (Index externalNodesNumber)
  deriving (Generic, NFDataX, Show)

data CPUState portsNumber nodesNumber edgesNumber agentType = CPUState
  { _phase :: Phase ((*) 2 portsNumber) nodesNumber
  , _memoryManager :: MemoryManager
  , _previousLoadedNode :: Maybe (LoadedNode portsNumber agentType)
  , _previousRamForm :: Maybe (RamForm portsNumber agentType)
  , _changes :: Map ((*) 2 portsNumber) (Changes portsNumber)
  , _interface :: Interface ((*) 2 portsNumber)
  , _nodesToWrite :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  , _rootNodeAddress :: AddressNumber
  , _activePair :: ActivePair portsNumber agentType
  -- ^ Active pair under reduction, stored by ReduceRule, consumed by ReduceDelta
  , _reduceRuleResult :: ReduceRuleResult nodesNumber edgesNumber portsNumber agentType
  -- ^ Result of the reduction rule, stored by ReduceRule, consumed by ReduceDelta
  , _delta :: Delta nodesNumber edgesNumber portsNumber agentType
  -- ^ Delta built from _reduceRuleResult, stored by ReduceDelta, consumed by ReduceInterface/ReduceCommit
  }
  deriving (Generic, NFDataX)

instance (Show (CPUState portsNumber nodesNumber edgesNumber agentType)) where
  show CPUState{..} =
    "phase = "
      P.++ show _phase
      P.++ "\nchanges = "
      P.++ show _changes
      P.++ "\ninterface"
      P.++ show _interface

initCPUState ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  ) =>
  MemoryManager ->
  AddressNumber ->
  CPUState portsNumber nodesNumber edgesNumber agentType
initCPUState initMM initRootNodeAddress =
  CPUState
    { _phase = Init
    , _memoryManager = initMM
    , _previousLoadedNode = def
    , _previousRamForm = def
    , _changes = def
    , _interface = def
    , _nodesToWrite = def
    , -- , _chooseReductionRule = reductionRule
      _rootNodeAddress = initRootNodeAddress
    , _activePair = errorX "initCPUState: _activePair is uninitialised"
    , _reduceRuleResult = errorX "initCPUState: _reduceRuleResult is uninitialised"
    , _delta = errorX "initCPUState: _delta is uninitialised"
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
  -- CPUState portsNumber nodesNumber edgesNumber agentType ->
  CPUIn portsNumber agentType ->
  State (CPUState portsNumber nodesNumber edgesNumber agentType) (CPUOut portsNumber agentType)
step i@(CPUIn processedLoadedNode) =
  do
    s@(CPUState{..}) <- get
    case _phase of
      Init -> do
        put s{_phase = FetchLeftActiveAddress}
        pure $ defaultOut _rootNodeAddress
      FetchLeftActiveAddress ->
        if isJust ramForm
          then do
            put s{_phase = DelayFetch, _previousRamForm = ramForm}
            pure CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
          else do
            put s{_phase = Done}
            pure $ defaultOut _rootNodeAddress
       where
        activeAddress = giveActiveAddressNumber _memoryManager
        ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
      FetchRightActiveAddress -> do
        put s{_phase = DelayReduce, _previousLoadedNode = processedLoadedNode, _previousRamForm = ramForm}
        pure CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
       where
        activeAddress =
          (view nodeAddress . (fromMaybe (errorX "Wrong definition of active pair") <$> view primaryPort))
            . view containedNode
            <$> processedLoadedNode
        ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
      ReduceRule -> do
        put
          s
            { _phase = ReduceDelta
            , _memoryManager = allocatedAddressesMemoryManager
            , _activePair = acPair
            , _reduceRuleResult = reduceRuleResult
            }
        pure $ defaultOut _rootNodeAddress
       where
        acPair = fromMaybe (errorX $ "cpu: 1. i = \n" P.++ show i) (ActivePair <$> _previousLoadedNode <*> processedLoadedNode)
        removedActivePairMemoryManager = removeActivePair acPair _memoryManager
        freeAddressesNumber =
          getAddressesAllocationCount @agentType @nodesNumber @edgesNumber @portsNumber
            (view (containedNode . nodeType) leftLoadedNode)
            (view (containedNode . nodeType) rightLoadedNode)
        (freeAddresses, allocatedAddressesMemoryManager) = giveAddresses freeAddressesNumber removedActivePairMemoryManager
        reduceRuleResult = makeReduction freeAddresses leftLoadedNode rightLoadedNode
        leftLoadedNode = fromMaybe (errorX "cpu: ReduceRule: no left node") _previousLoadedNode
        rightLoadedNode = fromMaybe (errorX "cpu: ReduceRule: no right node") processedLoadedNode
      ReduceDelta -> do
        put
          s
            { _phase = ReduceInterface
            , _memoryManager = writeNewActives delta _memoryManager
            , _delta = delta
            }
        pure $ defaultOut _rootNodeAddress
       where
        delta = toDelta @portsNumber @edgesNumber @nodesNumber _activePair _reduceRuleResult
      ReduceInterface -> do
        put
          s
            { _phase = ReduceCommit
            , _changes = allChanges
            , _interface = newInterface
            }
        pure $ defaultOut _rootNodeAddress
       where
        newInterface = getInterface @portsNumber @((*) 2 portsNumber) (_delta ^. activePair)
        allChanges = getAllChangesByDelta _delta newInterface
      ReduceCommit -> do
        put
          ( s
              { _phase = nextPhase
              , _previousLoadedNode = def
              , _nodesToWrite = _newNodes _delta
              , _rootNodeAddress = newRootAddress
              }
          )
        pure CPUOut{_ramForm = def, _nextRootNodeAddress = newRootAddress}
       where
        newRootAddress = changeRootNode (_delta ^. activePair) _rootNodeAddress _delta
        nextPhase
          | _interface == def = WriteNewNodes 0
          | otherwise = ReadExternal 0
      ReadExternal counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = (,def) <$> _interface !! counter
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase
          | _ramForm == def && counter == (maxBound :: Index ((*) 2 portsNumber)) || _interface == def = WriteNewNodes 0
          | _ramForm == def = ReadExternal $ counter + 1
          | otherwise = DelayWrite counter
      WriteChange counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = do
          ln <- processedLoadedNode
          address <- _interface !! counter
          let newNode = applyChangesToNode (_containedNode ln) <$> find _changes address
           in pure (address, Just (address, newNode))
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase =
          if counter == (maxBound :: Index ((*) 2 portsNumber))
            then WriteNewNodes 0
            else ReadExternal $ counter + 1
      WriteNewNodes counter -> do
        put s{_phase = nextPhase, _previousRamForm = _ramForm}
        pure CPUOut{..}
       where
        _ramForm = (\(LoadedNode n a) -> (a, Just (a, Just n))) <$> _nodesToWrite !! counter
        _nextRootNodeAddress = _rootNodeAddress
        nextPhase
          | counter == (maxBound :: Index nodesNumber) || (_nodesToWrite == def) = FetchLeftActiveAddress
          | otherwise = WriteNewNodes $ counter + 1
      Done -> pure $ defaultOut _rootNodeAddress
      DelayFetch -> do
        put s{_phase = FetchRightActiveAddress}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}
      DelayReduce -> do
        put s{_phase = ReduceRule}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}
      DelayWrite counter -> do
        put s{_phase = WriteChange counter}
        pure CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress}

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
