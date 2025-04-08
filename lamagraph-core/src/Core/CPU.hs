{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Core.CPU where

import Clash.Prelude
import Control.Lens
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

-- import RetroClash.Barbies
-- import RetroClash.CPU

data CPUIn cellsNumber nodesNumber edgesNumber portsNumber agentType = CPUIn
  { chooseReductionRule :: ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType
  , processedActivePair :: (Maybe AddressNumber, Maybe (ActivePair portsNumber agentType))
  , externalNodes :: Vec ((*) 2 portsNumber) (Maybe (LoadedNode portsNumber agentType))
  , rootNodeAddress :: AddressNumber
  }
  deriving (Generic, NFDataX, Show)

data CPUOut portsNumber agentType = CPUOut
  { _activePairAddress :: Maybe AddressNumber
  , _rootNodeAddress :: AddressNumber
  , _updatedExternalNodes :: Vec ((*) 2 portsNumber) (Maybe (LoadedNode portsNumber agentType))
  , _interface :: Interface ((*) 2 portsNumber)
  }
  deriving (Generic, Default, ShowX, Eq, Show, NFDataX)

data Phase
  = Init
  | ExtraInit (Unsigned 3)
  | WriteChanges1
  | WriteChanges2
  | GetInterface
  | FetchActivePair
  | Reduce
  | Done
  deriving (Generic, NFDataX, Show)

data CPUState cellsNumber agentType portsNumber = CPUState
  { _phase :: Phase
  , _memoryManager :: MemoryManager cellsNumber
  , _changes :: Maybe (Map ((*) 2 portsNumber) (Changes portsNumber))
  , _eNodes :: Maybe (Interface ((*) 2 portsNumber))
  }
  deriving (Generic, NFDataX)

$(makeLenses ''CPUState)

instance (Show (CPUState cellsNumber agentType portsNumber)) where
  show CPUState{..} =
    "phase = "
      P.++ show _phase
      P.++ "\nchanges = "
      P.++ show _changes
      P.++ "\neNoses"
      P.++ show _eNodes

defaultCPUState :: MemoryManager cellsNumber -> CPUState cellsNumber agentType portsNumber
defaultCPUState initMM = CPUState{_phase = Init, _memoryManager = initMM, _changes = def, _eNodes = def}

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
  ) =>
  CPUState cellsNumber agentType portsNumber ->
  CPUIn cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  (CPUState cellsNumber agentType portsNumber, CPUOut portsNumber agentType)
step s@(CPUState ph mm ch maybeExNodes) i@(CPUIn chooseReductionRule (maybeActiveAddress, maybeAcPair) exNodes rootAddress) = case ph of
  Init ->
    ( CPUState FetchActivePair mm def def
    , CPUOut maybeActiveAddress rootAddress def def
    )
  FetchActivePair ->
    if isJust activeAddress
      then
        ( CPUState (ExtraInit 0) mm def def
        , CPUOut activeAddress rootAddress def def
        )
      else
        ( CPUState Done mm def def
        , CPUOut maybeActiveAddress rootAddress def def
        )
   where
    activeAddress = giveActiveAddressNumberNoSignal mm
  Reduce ->
    ( CPUState (ExtraInit 2) allocatedAddressesMemoryManager (Just allChanges) (Just interface)
    , CPUOut maybeActiveAddress newRootAddress def interface
    )
   where
    acPair = fromMaybe (errorX $ "cpu: 1. i = \n" P.++ show i) maybeAcPair
    removedActivePairMemoryManager = removeActivePairNoSignal acPair mm
    (delta, allocatedAddressesMemoryManager) = reduceNoSignal chooseReductionRule removedActivePairMemoryManager acPair
    interface = getInterface @portsNumber @((*) 2 portsNumber) acPair
    allChanges = getAllChangesByDeltaNoSignal delta interface
    newRootAddress = changeRootNode acPair rootAddress delta
  GetInterface ->
    ( CPUState WriteChanges1 mm ch maybeExNodes
    , CPUOut
        def
        rootAddress
        updatedExNodes
        (fromMaybe (errorX $ "cpu: 5\nstate=\n" P.++ show s) maybeExNodes)
    )
   where
    updatedExNodes = maybe (errorX "cpu: 2") (updateLoadedNodesByChanges exNodes) ch
  WriteChanges1 ->
    ( CPUState FetchActivePair mm ch maybeExNodes
    , CPUOut def rootAddress updatedExNodes (fromMaybe (errorX $ "cpu: 5\nstate=\n" P.++ show s) maybeExNodes)
    )
   where
    updatedExNodes = maybe (errorX "cpu: 5") (updateLoadedNodesByChanges exNodes) ch
  WriteChanges2 ->
    ( CPUState FetchActivePair mm ch maybeExNodes
    , CPUOut def rootAddress updatedExNodes (fromMaybe (errorX $ "cpu: 5\nstate=\n" P.++ show s) maybeExNodes)
    )
   where
    updatedExNodes = maybe (errorX "cpu: 5") (updateLoadedNodesByChanges exNodes) ch
  Done ->
    ( CPUState Done mm def maybeExNodes
    , CPUOut maybeActiveAddress rootAddress def def
    )
  ExtraInit a ->
    ( CPUState nextPhase mm ch maybeExNodes
    , CPUOut maybeActiveAddress rootAddress def interface
    )
   where
    interface = if a == 2 then fromMaybe (errorX $ "cpu: 4\nstate=\n" P.++ show s) maybeExNodes else def
    nextPhase
      | a == 0 = ExtraInit (a + 1)
      | a == 1 = Reduce
      | otherwise = GetInterface

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
  ) =>
  MemoryManager cellsNumber ->
  Signal dom (CPUIn cellsNumber nodesNumber edgesNumber portsNumber agentType) ->
  Signal dom (CPUOut portsNumber agentType)
mealyCore initialMM = mealy step (defaultCPUState initialMM)

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
  -- Ram dom portsNumber agentType ->
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  AddressNumber ->
  MemoryManager cellsNumber -> -- Initial information about busy addresses and active pairs
  ChooseReductionRule cellsNumber nodesNumber edgesNumber portsNumber agentType ->
  -- Signal dom AddressNumber
  Signal dom (CPUOut portsNumber agentType)
logicBroad initialNet initialRootNodeAddress initialMemoryManager chooseReductionRule = o
 where
  ram = exposeEnable (blockRam initialNet)
  initialCPUIn = CPUIn chooseReductionRule def def initialRootNodeAddress

  i = register initialCPUIn nextInput
  o = mealyCore initialMemoryManager i

  maybeActiveAddress = _activePairAddress <$> o
  interface = _interface <$> o
  maybeUpdatedExternalNodes = _updatedExternalNodes <$> o
  root = _rootNodeAddress <$> o

  -- load `ActivePair` and clean up it
  nextProcessActivePair = loadActivePair ram maybeActiveAddress
  -- load external `Node`s by it's addresses
  !loadedInterface = loadInterface ram interface maybeUpdatedExternalNodes

  -- write accumulated changes to the ram
  -- !updatedExternalNodes = bundle (writeChanges ram maybeUpdatedExternalNodes)

  nextInput = CPUIn chooseReductionRule <$> bundle (maybeActiveAddress, nextProcessActivePair) <*> loadedInterface <*> root
