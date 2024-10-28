module Core.Reducer where

import Clash.Prelude
import Control.Lens (makeLenses, set, (^.))
import Core.MemoryManager
import Core.Node

type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3

{- | Regulates which external (interface) `Ports` belonged to which `Node` (and in which place) before the reduction.
  This is necessary in order to be able to coordinate the interface in the reducer
-}
data OldId portsNumber = LeftLoadedNode (IdOfPort portsNumber) | RightLoadedNode (IdOfPort portsNumber)

-- | Result of abstract reduction rule
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (OldId portsNumber, OldId portsNumber))
  , _nodes ::
      Vec nodesNumber (Maybe (LocalNode portsNumber, Vec portsNumber (Maybe (OldId portsNumber, IdOfPort portsNumber))))
  }

$(makeLenses ''ReduceRuleResult)

{- | Coordinates interface `Port` in `LocalNode`.
I.e. it connects the "hanging" ports from the reduction rule with the `LoadedNode` from the real net
-}
putInterfacesNodeToGlobalNet ::
  forall portsNumber.
  (KnownNat portsNumber) =>
  LoadedNode portsNumber ->
  LoadedNode portsNumber ->
  (LocalNode portsNumber, Vec portsNumber (Maybe (OldId portsNumber, IdOfPort portsNumber))) ->
  LocalNode portsNumber
putInterfacesNodeToGlobalNet leftLNode rightLNode (localNode, interfacePortsInfo) = foldl replaceOnePort localNode interfacePortsInfo
 where
  replaceOnePort :: LocalNode portsNumber -> Maybe (OldId portsNumber, IdOfPort portsNumber) -> LocalNode portsNumber
  replaceOnePort ln maybePortInfo =
    case maybePortInfo of
      Nothing -> ln
      Just (oldPortId, portId) ->
        let setPort loadedN idOfLoadedNode = case getPortById (loadedN ^. containedNode) idOfLoadedNode of
              Nothing -> error "Port must be connected"
              Just portOfLoadedNode -> case portId of
                Primary -> set numberedNode (set primaryPort portOfLoadedNode (ln ^. numberedNode)) ln
                Id index ->
                  set
                    numberedNode
                    (set secondaryPorts (replace index (Just portOfLoadedNode) (ln ^. numberedNode . secondaryPorts)) (ln ^. numberedNode))
                    ln
         in case oldPortId of
              LeftLoadedNode i -> setPort leftLNode i
              RightLoadedNode i -> setPort rightLNode i

{- | Coordinates interface `Port` in disappeared `Node`.
I.e. it connects relevant external `LoadedNode` together (making `Edge`)
-}
putInterfacesEdgeToGlobalNet ::
  forall portsNumber.
  (KnownNat portsNumber) =>
  LoadedNode portsNumber ->
  LoadedNode portsNumber ->
  Maybe (OldId portsNumber, OldId portsNumber) ->
  Maybe (Edge portsNumber)
putInterfacesEdgeToGlobalNet leftLNode rightLNode info = case info of
  Nothing -> Nothing
  Just (leftEndPortInfo, rightEndPortInfo) -> Just $ Edge lEnd rEnd
   where
    constructEnd portInfo = case portInfo of
      LeftLoadedNode portId -> EdgeEnd (leftLNode ^. originalAddress) portId
      RightLoadedNode portId -> EdgeEnd (rightLNode ^. originalAddress) portId
    lEnd = constructEnd leftEndPortInfo
    rEnd = constructEnd rightEndPortInfo

reducer ::
  forall dom portsNumber nodesNumber edgesNumber.
  (KnownDomain dom, KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber) =>
  ((NodeTag, NodeTag) -> ReduceRuleResult nodesNumber edgesNumber portsNumber) ->
  Signal dom (ActivePair portsNumber) ->
  Signal dom (Delta nodesNumber edgesNumber portsNumber)
reducer transFunction activeP = Delta <$> nodesForDelta <*> edgesForDelta <*> activeP
 where
  leftLNode = (^. leftNode) <$> activeP
  rightLNode = (^. rightNode) <$> activeP
  reduceRuleRes = transFunction <$> bundle ((^. containedNode . nodeType) <$> leftLNode, (^. containedNode . nodeType) <$> rightLNode)
  nodesForDelta =
    bundle $
      map
        ( \signalMaybeNodesInterfaceInfo -> case sequenceA signalMaybeNodesInterfaceInfo of
            Nothing -> pure Nothing
            Just signalNodesInterfaceInfo -> sequenceA (Just (putInterfacesNodeToGlobalNet <$> leftLNode <*> rightLNode <*> signalNodesInterfaceInfo))
        )
        (unbundle $ (^. nodes) <$> reduceRuleRes)
  edgesForDelta =
    bundle $ map (putInterfacesEdgeToGlobalNet <$> leftLNode <*> rightLNode <*>) (unbundle $ (^. edges) <$> reduceRuleRes)
