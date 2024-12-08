{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}

module Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta) where

import Clash.Prelude
import Control.Lens hiding (ifoldl)
import Core.Map
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer

-- | Type alias for triple (the address of the external node to update, the id of the port to update, the port to update to)
type UpdateInfo (portsNumber :: Nat) = (AddressNumber, IdOfPort portsNumber, Connection portsNumber)

{- | Generate update information in attached external `Node`.
The result can be interpreted as
- `Just` `UpdateInfo`
- `Nothing` if `Node` is not in `Interface`
-}
getUpdateInfoByPort ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  AddressNumber ->
  Interface maxNumOfChangedNodes ->
  Connection portsNumber ->
  IdOfPort portsNumber ->
  Maybe (UpdateInfo portsNumber)
getUpdateInfoByPort addressPointTo interface port portId =
  case port of
    Connected (Port addressNumber connectedToPortId) -> if Just addressNumber `elem` interface then update addressNumber else def
     where
      newPort = Connected $ Port addressPointTo portId
      update addressNum = Just (addressNum, connectedToPortId, newPort)
    NotConnected -> Nothing

{- | The same as `updatePortsInfoByPort`, but updating happens by `Edge`.
In fact it accumulate info about which external `LoadedNode` have become connected with each other
-}
getUpdateInfoByEdge ::
  forall portsNumber.
  (KnownNat portsNumber) =>
  Edge portsNumber ->
  ( Maybe (UpdateInfo portsNumber)
  , Maybe (UpdateInfo portsNumber)
  )
getUpdateInfoByEdge (Edge maybeLeftEdge maybeRightEdge) =
  (leftUpdate, rightUpdate)
 where
  leftUpdate = case maybeRightEdge of
    Connected (Port address portId) -> Just (address, portId, maybeLeftEdge)
    NotConnected -> Nothing
  rightUpdate = case maybeLeftEdge of
    Connected (Port address portId) -> Just (address, portId, maybeRightEdge)
    NotConnected -> Nothing

-- | Insert update information into `Map`
insertInInfoVec ::
  (KnownNat size, KnownNat portsNumber) =>
  Map size (Changes portsNumber) ->
  Maybe (UpdateInfo portsNumber) ->
  Map size (Changes portsNumber)
insertInInfoVec infoVec maybeInsertionInfo =
  case maybeInsertionInfo of
    Nothing -> infoVec
    Just (address, portId, newPort) ->
      insertWith
        infoVec
        (insertFunction portId newPort)
        address
 where
  insertFunction portId newPort maybeInfo = Just $ maybe (constructNewInfo def) constructNewInfo maybeInfo
   where
    constructNewInfo info@(Changes secPortsInfo _) = case portId of
      Primary -> set primeP (Just newPort) info
      Id i -> set secP (replace i (Just newPort) secPortsInfo) info

-- | Accumulate all changes by ports of the given `Node` and write it in the `Map`
accumulatePortsChangesByLoadedNode ::
  forall portsNumber maxNumOfChangedNodes.
  (KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (Changes portsNumber) ->
  Interface maxNumOfChangedNodes ->
  LoadedNode portsNumber ->
  Map maxNumOfChangedNodes (Changes portsNumber)
accumulatePortsChangesByLoadedNode infoVec interface signalLoadedNode =
  let updateToConcreteNode = getUpdateInfoByPort signalAddress interface
      signalAddress = view originalAddress signalLoadedNode
      infoVecByPrimary =
        insertInInfoVec
          infoVec
          ( updateToConcreteNode
              (view (containedNode . primaryPort) signalLoadedNode)
              Primary
          )
   in ifoldl
        ( \oldInfoVec i signalMaybePort -> case signalMaybePort of
            Nothing -> oldInfoVec
            Just signalPort -> insertInInfoVec oldInfoVec (updateToConcreteNode signalPort (Id i))
        )
        infoVecByPrimary
        (view (containedNode . secondaryPorts) signalLoadedNode)

-- | Accumulate all changes by new `Node`s from `Delta`
accumulateUpdatesByNodes ::
  (KnownNat nodesNumber, KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (Changes portsNumber) ->
  Interface maxNumOfChangedNodes ->
  Vec nodesNumber (Maybe (LoadedNode portsNumber)) ->
  Map maxNumOfChangedNodes (Changes portsNumber)
accumulateUpdatesByNodes infoVec interface loadedNodes =
  foldl
    ( \oldInfoVec maybeLoadedNode -> case maybeLoadedNode of
        Nothing -> oldInfoVec
        Just loadedNode -> accumulatePortsChangesByLoadedNode oldInfoVec interface loadedNode
    )
    infoVec
    loadedNodes

-- | Insert update information of the `Edge` into the `Map`
insertPairInInfoVec ::
  (KnownNat size, KnownNat portsNumber) =>
  Map size (Changes portsNumber) ->
  (Maybe (UpdateInfo portsNumber), Maybe (UpdateInfo portsNumber)) ->
  Map size (Changes portsNumber)
insertPairInInfoVec oldInfoVec (maybeLeftUpdateInfo, maybeRightUpdateInfo) =
  insertInInfoVec (insertInInfoVec oldInfoVec maybeLeftUpdateInfo) maybeRightUpdateInfo

-- | Accumulate all changes by `Edge`s from `Delta`
accumulateUpdatesByEdges ::
  (KnownNat edgesNumber, KnownNat portsNumber, KnownNat maxNumOfChangedNodes) =>
  Map maxNumOfChangedNodes (Changes portsNumber) ->
  Vec edgesNumber (Maybe (Edge portsNumber)) ->
  Map maxNumOfChangedNodes (Changes portsNumber)
accumulateUpdatesByEdges infoVec edgesForUpdate =
  foldl
    ( \oldInfoVec maybeEdge -> case maybeEdge of
        Nothing -> oldInfoVec
        Just edge -> insertPairInInfoVec oldInfoVec (getUpdateInfoByEdge edge)
    )
    infoVec
    edgesForUpdate

-- | Get all changes from `Delta`
getAllChangesByDelta ::
  (KnownNat edgesNumber, KnownNat nodesNumber, KnownNat maxNumOfChangedNodes, KnownNat portsNumber, KnownDomain dom) =>
  Signal dom (Delta nodesNumber edgesNumber portsNumber) ->
  Signal dom (Interface maxNumOfChangedNodes) ->
  Signal dom (Map maxNumOfChangedNodes (Changes portsNumber))
getAllChangesByDelta delta interface = accumulateUpdatesByNodes <$> (accumulateUpdatesByEdges def <$> edgesForUpdate) <*> interface <*> nodesForUpdate
 where
  edgesForUpdate = view newEdges <$> delta
  nodesForUpdate = view newNodes <$> delta
