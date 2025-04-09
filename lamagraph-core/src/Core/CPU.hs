{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.CPU where

import Clash.Prelude
import Control.Lens hiding (Index)
import Core.Loader
import Core.Map
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDeltaNoSignal)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe (fromMaybe, isJust)
import INet.Net
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
  | Reduce
  | ReadExternal (Index externalNodesNumber)
  | WriteChange (Index externalNodesNumber)
  | WriteNewNodes (Index newNodesNumber)
  | Done
  | Delay (Phase externalNodesNumber newNodesNumber)
  deriving (Generic, NFDataX, Show)

data CPUState cellsNumber portsNumber nodesNumber edgesNumber agentType = CPUState
  { _phase :: Phase ((*) 2 portsNumber) nodesNumber
  , _memoryManager :: MemoryManager cellsNumber
  , _previousLoadedNode :: Maybe (LoadedNode portsNumber agentType)
  , _previousRamForm :: Maybe (RamForm portsNumber agentType)
  , _changes :: Map ((*) 2 portsNumber) (Changes portsNumber)
  , _interface :: Interface ((*) 2 portsNumber)
  , _nodesToWrite :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  , _chooseReductionRule :: ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType
  , _rootNodeAddress :: AddressNumber
  }
  deriving (Generic, NFDataX)

instance (Show (CPUState cellsNumber portsNumber nodesNumber edgesNumber agentType)) where
  show CPUState{..} =
    "phase = "
      P.++ show _phase
      P.++ "\nchanges = "
      P.++ show _changes
      P.++ "\ninterface"
      P.++ show _interface

initCPUState ::
  (KnownNat portsNumber, KnownNat nodesNumber) =>
  MemoryManager cellsNumber ->
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  AddressNumber ->
  CPUState cellsNumber portsNumber nodesNumber edgesNumber agentType
initCPUState initMM reductionRule initRootNodeAddress =
  CPUState
    { _phase = Init
    , _memoryManager = initMM
    , _previousLoadedNode = def
    , _previousRamForm = def
    , _changes = def
    , _interface = def
    , _nodesToWrite = def
    , _chooseReductionRule = reductionRule
    , _rootNodeAddress = initRootNodeAddress
    }

-- TODO: make it with State Monad
step ::
  forall portsNumber nodesNumber edgesNumber cellsNumber agentType.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  , Show agentType
  , Eq agentType
  ) =>
  CPUState cellsNumber portsNumber nodesNumber edgesNumber agentType ->
  CPUIn portsNumber agentType ->
  (CPUState cellsNumber portsNumber nodesNumber edgesNumber agentType, CPUOut portsNumber agentType)
step s@(CPUState{..}) i@(CPUIn processedLoadedNode) = case _phase of
  Init ->
    (s{_phase = FetchLeftActiveAddress}, defaultOut _rootNodeAddress)
  FetchLeftActiveAddress ->
    if isJust ramForm
      then
        ( s{_phase = Delay FetchRightActiveAddress, _previousRamForm = ramForm}
        , CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
        )
      else (s{_phase = Done}, defaultOut _rootNodeAddress)
   where
    activeAddress = giveActiveAddressNumberNoSignal _memoryManager
    ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
  FetchRightActiveAddress ->
    ( s{_phase = Delay Reduce, _previousLoadedNode = processedLoadedNode, _previousRamForm = ramForm}
    , CPUOut{_ramForm = ramForm, _nextRootNodeAddress = _rootNodeAddress}
    )
   where
    activeAddress =
      (view nodeAddress . (fromMaybe (errorX "Wrong definition of active pair") <$> view primaryPort))
        . view containedNode
        <$> processedLoadedNode
    ramForm = (\address -> (address, Just (address, Nothing))) <$> activeAddress
  Reduce ->
    ( s
        { _phase = ReadExternal 0
        , _memoryManager = allocatedAddressesMemoryManager
        , _previousLoadedNode = def
        , _changes = allChanges
        , _interface = newInterface
        , _nodesToWrite = _newNodes delta
        , _rootNodeAddress = newRootAddress
        }
    , CPUOut{_ramForm = def, _nextRootNodeAddress = newRootAddress}
    )
   where
    acPair = fromMaybe (errorX $ "cpu: 1. i = \n" P.++ show i) (ActivePair <$> _previousLoadedNode <*> processedLoadedNode)
    removedActivePairMemoryManager = removeActivePairNoSignal acPair _memoryManager
    (delta, allocatedAddressesMemoryManager) = reduceNoSignal _chooseReductionRule removedActivePairMemoryManager acPair
    newInterface = getInterface @portsNumber @((*) 2 portsNumber) acPair
    allChanges = getAllChangesByDeltaNoSignal delta newInterface
    newRootAddress = changeRootNode acPair _rootNodeAddress delta
  ReadExternal counter ->
    (s{_phase = nextPhase, _previousRamForm = _ramForm}, CPUOut{..})
   where
    _ramForm = (,def) <$> _interface !! counter
    _nextRootNodeAddress = _rootNodeAddress
    nextPhase
      | _ramForm == def && counter == (maxBound :: Index ((*) 2 portsNumber)) || _interface == def = WriteNewNodes 0
      | _ramForm == def = ReadExternal $ counter + 1
      | otherwise = Delay (WriteChange counter)
  WriteChange counter ->
    (s{_phase = nextPhase, _previousRamForm = _ramForm}, CPUOut{..})
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
  WriteNewNodes counter -> (s{_phase = nextPhase, _previousRamForm = _ramForm}, CPUOut{..})
   where
    _ramForm = (\(LoadedNode n a) -> (a, Just (a, Just n))) <$> _nodesToWrite !! counter
    _nextRootNodeAddress = _rootNodeAddress
    nextPhase
      | counter == (maxBound :: Index nodesNumber) || (_nodesToWrite == def) = FetchLeftActiveAddress
      | otherwise = WriteNewNodes $ counter + 1
  Done -> (s, defaultOut _rootNodeAddress)
  Delay ph -> (s{_phase = ph}, CPUOut{_ramForm = _previousRamForm, _nextRootNodeAddress = _rootNodeAddress})

mealyCore ::
  forall portsNumber nodesNumber edgesNumber cellsNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , Show agentType
  , NFDataX agentType
  , Eq agentType
  ) =>
  MemoryManager cellsNumber ->
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  AddressNumber ->
  Signal dom (CPUIn portsNumber agentType) ->
  Signal dom (CPUOut portsNumber agentType)
mealyCore initialMM reductionRule initRootNodeAddress = mealy step (initCPUState initialMM reductionRule initRootNodeAddress)

logicBroad ::
  forall portsNumber nodesNumber edgesNumber cellsNumber agentType dom.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  , KnownDomain dom
  , HiddenClockResetEnable dom
  , NFDataX agentType
  , Show agentType
  , Eq agentType
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  AddressNumber ->
  MemoryManager cellsNumber -> -- Initial information about busy addresses and active pairs
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  Signal dom AddressNumber
logicBroad initialNet initialRootNodeAddress initialMemoryManager chooseReductionRule = _nextRootNodeAddress <$> o
 where
  ram = exposeEnable (blockRam initialNet)
  initialCPUIn = CPUIn def

  i = register initialCPUIn nextInput
  o = mealyCore initialMemoryManager chooseReductionRule initialRootNodeAddress i

  nextLoadedNode = delayedMaybeRam ram (_ramForm <$> o)

  nextInput = CPUIn <$> (makeLoadedNodeFromRamForm <$> nextLoadedNode <*> (_ramForm <$> o))

  makeLoadedNodeFromRamForm n ramForm = LoadedNode <$> n <*> (fst <$> ramForm)
