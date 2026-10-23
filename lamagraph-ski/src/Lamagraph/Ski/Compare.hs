{- | Scrutiny of reduced SKI normal forms: structural statistics (node-type
histogram, edge count, fingerprint) and a logical node count that expands
@D@ (duplicator) sharing. The full canonical-form comparison is
productionised in S7.
-}
module Lamagraph.Ski.Compare (
  nodeHistogram,
  edgeCount,
  fingerprint,
  countLogical,
) where

import Relude

import Data.IntMap qualified as IntMap
import Data.Map qualified as Map

import GraphRewriting (
  Graph,
  Node,
  Port,
  Rewrite,
  adverseNodes,
  edgeMap,
  evalGraph,
  inspectNode,
  nodeMap,
  readNode,
  readNodeList,
 )
import INet.Graph (SKI (..))

-- | Name of a SKI node constructor (for histograms / debugging).
nodeName :: SKI -> String
nodeName = \case
  R{} -> "R"
  A{} -> "A"
  I{} -> "I"
  E{} -> "E"
  D{} -> "D"
  V{} -> "V"
  K0{} -> "K0"
  K1{} -> "K1"
  S0{} -> "S0"
  S1{} -> "S1"
  S2{} -> "S2"

-- | Count of each node type in the graph.
nodeHistogram :: Graph SKI -> Map.Map String Int
nodeHistogram g =
  foldl'
    (\m n -> Map.insertWith (+) (nodeName n) 1 m)
    Map.empty
    (IntMap.elems (nodeMap g))

-- | Number of edges (hyperedges) in the graph.
edgeCount :: Graph SKI -> Int
edgeCount g = IntMap.size (edgeMap g)

{- | A cheap structural fingerprint: (node-type histogram, edge count). Equal
normal forms must share a fingerprint (necessary, not sufficient — the full
canonical comparison lands in S7).
-}
fingerprint :: Graph SKI -> (Map.Map String Int, Int)
fingerprint g = (nodeHistogram g, edgeCount g)

{- | Count the logical occurrences of nodes satisfying @match@, starting from
the root (@R@ node) and expanding @D@ (duplicator) nodes so that a shared
subgraph is counted once per logical use. Interaction nets use @D@ nodes for
sharing, so a single physical node can stand for several logical occurrences;
counting physical nodes would undercount. Assumes the graph is a directed
acyclic graph, as a reduced normal form is.
-}
countLogical :: (SKI -> Bool) -> Graph SKI -> Int
countLogical match = evalGraph trav
  where
  trav :: Rewrite SKI Int
  trav = do
    r <- findR
    case r of
      Nothing -> return 0
      Just rn -> do
        ports <- inspectNode rn
        case ports of
          [outPort] -> go rn outPort
          _ -> return 0

  findR :: Rewrite SKI (Maybe Node)
  findR = do
    ns <- readNodeList
    findRGo ns
   where
    findRGo [] = return Nothing
    findRGo (n : rest) = do
      v <- readNode n
      case v of
        R{} -> return (Just n)
        _ -> findRGo rest

  go :: Node -> Port -> Rewrite SKI Int
  go from port = do
    ts <- adverseNodes from port
    case ts of
      [t] -> do
        v <- readNode t
        case v of
          A{out1 = e1, out2 = e2} -> (+) <$> go t e1 <*> go t e2
          D{out = eo} -> go t eo
          _ -> return (if match v then 1 else 0)
      _ -> return 0
