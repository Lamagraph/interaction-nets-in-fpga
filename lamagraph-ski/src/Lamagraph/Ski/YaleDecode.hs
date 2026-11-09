{- | Decoding helpers for a normalized YALE (Mackie 1998) lambda interaction
net.

The extensional tests do not rely on a full structural decode; they read the
surviving root marker via 'rootConstantNameYale' (whichever of two distinct
marker constants a probed Church boolean selects stays at the root).
-}
module Lamagraph.Ski.YaleDecode (
  rootConstantNameYale,
  countMarkersYale,
) where

import Relude

import Data.Set qualified as Set
import GraphRewriting (
  Edge,
  Graph,
  Node,
  Rewrite,
  attachedNodes,
  evalGraph,
  readNode,
  readNodeList,
 )

import Lamagraph.Ski.YaleNet (YaleNode (..))

-- * Root helpers

findRoot :: Graph YaleNode -> Rewrite YaleNode Node
findRoot _ = do
  ns <- readNodeList
  go ns
 where
  go (n : ns) = do
    v <- readNode n
    if isInit v then return n else go ns
  go [] = error "findRoot: no Initiator node"
  isInit (Initiator{}) = True
  isInit _ = False

rootOut :: Node -> Rewrite YaleNode Edge
rootOut r = do
  v <- readNode r
  case v of
    Initiator{out = e} -> return e
    _ -> error "rootOut: not an Initiator node"

-- * Edge resolution

nodesOn :: Edge -> Rewrite YaleNode [YaleNode]
nodesOn e = do
  ns <- attachedNodes e
  mapM readNode ns

{- | Whether edge @e@ (with attached nodes @ns@) carries a real term node: a
'YaleLam', a 'YaleApp', or a 'Constant'. A 'Copy' is not real (resolution
passes through it).
-}
isReal :: Edge -> [YaleNode] -> Bool
isReal e =
  any
    ( \case
        YaleLam{lamResult = a, lamVar = c, lamBody = b} -> a == e || b == e || c == e
        YaleApp{appFunc = a, appArg = b, appResult = c} -> a == e || b == e || c == e
        Constant{inp = i} -> i == e
        _ -> False
    )

{- | The 'Copy'-graph neighbors of edge @e@: the other two ports of any 'Copy'
that @e@ belongs to (all three ports carry the same sub-term, so they are
mutually connected).
-}
dupNeighbors :: [YaleNode] -> Edge -> [Edge]
dupNeighbors ns e =
  [ tgt
  | Copy{copyIn = p, copyOut1 = a1, copyOut2 = a2} <- ns
  , let ports = [p, a1, a2]
  , e `elem` ports
  , tgt <- ports
  , tgt /= e
  ]

{- | Resolve an edge to the nearest edge that carries a real term node,
searching through the 'Copy' auxiliary graph. Returns the original edge if no
real term node is reachable.
-}
resolve :: Edge -> Rewrite YaleNode Edge
resolve e0 = bfs [e0] Set.empty
 where
  bfs [] _ = return e0
  bfs (front : rest) seen
    | front `Set.member` seen = bfs rest seen
    | otherwise = do
        ns <- nodesOn front
        if isReal front ns
          then return front
          else
            let newSeen = Set.insert front seen
             in bfs (dupNeighbors ns front ++ rest) newSeen

-- * Marker counting

-- | Count the 'Constant' nodes named @name@ in a reduced YALE net.
countMarkersYale :: Graph YaleNode -> String -> Int
countMarkersYale g name = evalGraph go g
 where
  go = do
    ns <- readNodeList
    mapM readNode ns
      <&> length
      . filter
        ( \case
            Constant{name = nm} -> nm == name
            _ -> False
        )

-- * Root marker

{- | The name of the 'Constant' node at the root's output edge, if any,
resolving through 'Copy' agents. Used to read the surviving marker after a
Church boolean has been probed with two distinct marker constants.
-}
rootConstantNameYale :: Graph YaleNode -> Maybe String
rootConstantNameYale g = evalGraph go g
 where
  go = do
    r <- findRoot g
    e0 <- rootOut r
    e <- resolve e0
    ns <- nodesOn e
    case [nm | Constant{inp = i, name = nm} <- ns, i == e] of
      (nm : _) -> return (Just nm)
      [] -> return Nothing
