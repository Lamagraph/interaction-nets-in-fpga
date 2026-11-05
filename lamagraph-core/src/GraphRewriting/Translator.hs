{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module GraphRewriting.Translator (translate, AgentMaps, agentToAgent) where

import Clash.Prelude as Clash (
  Enum (fromEnum, toEnum),
  Index,
  KnownNat,
  SNat (SNat),
  Vec,
  def,
  fromSNat,
  replace,
  type (+),
 )
import qualified Clash.Sized.Vector as Clash
import Control.Monad (forM_)
import Control.Monad.Reader
import Control.Monad.State
import Core.Node
import Data.List (delete, elemIndex)
import qualified Data.List as Prelude
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GraphRewriting as GR
import GraphRewriting.Pattern.InteractionNet as GRINet
import qualified GraphRewriting.Pattern.InteractionNet as GR
import INet.Net as CoreINet
import Prelude

-- | Maps graph rewriting node type with INet.Net agent type
class (GRINet.INet n) => AgentMaps n agentType where
  agentToAgent :: n -> agentType

-- | State type: result net and memory map. Reader for graph rewriting functions
type GraphTranslator portsNumber agentType n =
  StateT
    (Vec CellsNumber (Maybe (Core.Node.Node portsNumber agentType)))
    ( StateT
        (Map.Map GR.Node AddressNumber, AddressNumber)
        (Reader (GR.Graph n))
    )

-- | Maps graph rewriting index into fpga memory
memoryMap ::
  GR.Node -> GraphTranslator portsNumber agentType n AddressNumber
memoryMap indexInGraph = do
  (m, newAddr) <- lift get
  case Map.lookup indexInGraph m of
    Nothing -> do
      lift $ put (Map.insert indexInGraph newAddr m, newAddr + 1)
      pure newAddr
    Just addr -> pure addr

{- | Make id of port in INet.Node by GR.Port and GR.Node.
It finds respective number of hyperedge (actually, GR.Port) in own ports
-}
constructIdOfPort ::
  ( GRINet.INet n
  , View [GR.Port] n
  , KnownNat portsNumber
  , Show n
  ) =>
  GR.Port ->
  GR.Node ->
  GraphTranslator portsNumber agentType n (Core.Node.IdOfPort portsNumber)
constructIdOfPort edgeKey nodeKey = do
  grNode <- GR.readNode nodeKey
  let principalPortKey = GRINet.principalPort grNode
      secondaryPortsKeys = delete principalPortKey $ GR.inspect grNode :: [GR.Port]
  if principalPortKey == edgeKey
    then pure Primary
    else
      let i =
            maybe
              (error $ "there is no such Port in the Node: " ++ show grNode ++ " , " ++ show edgeKey)
              Clash.toEnum
              (elemIndex edgeKey secondaryPortsKeys)
       in pure $ Id i

{- | Make INet.Port in INet.Node by respective GR.Node and hyperedge.
GR.Node is necessary for distinguish "from" port and "to" port (it defines "from") in hyperedge
-}
hyperedgeToPort ::
  forall n portsNumber agentType.
  ( GRINet.INet n
  , View [GR.Port] n
  , KnownNat portsNumber
  , Show n
  ) =>
  GR.Node -> GR.Edge -> GraphTranslator portsNumber agentType n (Core.Node.Connection portsNumber)
hyperedgeToPort grNodeKey hyperEdge = do
  grNodes <- GR.attachedNodes hyperEdge
  case grNodes of
    [] -> error "empty hyperedge"
    [_] -> pure NotConnected
    [x, y] ->
      if x == grNodeKey
        then makeConnection y
        else makeConnection x
    _ -> error "there are more than two ports in hyperedge"
 where
  makeConnection n = do
    (idOfPort :: (Core.Node.IdOfPort portsNumber)) <- constructIdOfPort hyperEdge n
    addr <- memoryMap n
    pure $ Connected $ Core.Node.Port addr idOfPort

{- | Helper function for creating `Vec` of consistent size from a list of values.
It fills the empty cells with `Nothing`
-}
constructVec ::
  forall portsNumber a. (KnownNat portsNumber) => [a] -> Vec portsNumber (Maybe a)
constructVec xs = Clash.unsafeFromList $ (Just <$> xs) ++ nothings
 where
  vecLen = Clash.fromEnum (Clash.fromSNat $ Clash.SNat @portsNumber :: Index (portsNumber + 1))
  nothings = replicate (vecLen - Prelude.length xs) Nothing

-- | Maps GR.Node in the graph in INet.Node
translateNode ::
  forall n portsNumber agentType.
  ( View [GR.Port] n
  , GRINet.INet n
  , KnownNat portsNumber
  , Show n
  , AgentMaps n agentType
  ) =>
  GR.Node -> GraphTranslator portsNumber agentType n (Core.Node.Node portsNumber agentType)
translateNode grNodeKey = do
  grNode <- GR.readNode grNodeKey
  edgesKeys <- attachedEdges grNodeKey
  (sp :: [(Connection portsNumber, GR.Edge)]) <-
    mapM
      ( \e -> do
          c <- hyperedgeToPort grNodeKey e
          pure (c, e)
      )
      edgesKeys
  let prPort = GR.principalPort grNode
      _secondaryPorts = constructVec (map fst $ filter ((/= prPort) . snd) sp)
      _primaryPort =
        fst $
          fromMaybe (error $ "there is no primary port in the graph. connections:\n" ++ show sp ++ "node:\n" ++ show grNode) $
            Prelude.find
              ((== prPort) . snd)
              sp
      _nodeType = agentToAgent grNode
  pure Core.Node.Node{..}

-- | Maps GR.Graph in INet.Net. It Stateful function, so result Net is in the State
translateGraph ::
  forall n portsNumber agentType.
  ( View [GR.Port] n
  , GRINet.INet n
  , KnownNat portsNumber
  , Show n
  , AgentMaps n agentType
  ) =>
  GraphTranslator portsNumber agentType n ()
translateGraph = do
  grNodes <- readNodeList
  forM_
    grNodes
    ( \grNode -> do
        inetNode <- translateNode grNode
        addr <- memoryMap grNode
        addNodeToInet inetNode addr
    )
 where
  addNodeToInet n addr = modify (Clash.replace i (Just n))
   where
    i = Clash.fromEnum addr

-- | Translate GR.Graph into INet.Net
translate ::
  forall n agentType portsNumber.
  ( KnownNat portsNumber
  , GRINet.INet n
  , View [GR.Port] n
  , Show n
  , AgentMaps n agentType
  ) =>
  GR.Graph n -> Net portsNumber agentType -- alias for `Vec CellsNumber (Maybe (Core.Node.Node portsNumber agentType))`
translate graph = net
 where
  emptyNetDefined = runStateT translateGraph def
  emptyMemoryDefined = runStateT emptyNetDefined (Map.empty, 0)
  ((_, net), _) = runReader emptyMemoryDefined graph
