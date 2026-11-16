{- | Serialize a YALE interaction net to the JSON format understood by the
"interaction-nets-drawing" tools (<https://github.com/Lamagraph/interaction-nets-drawing>):
INviz (static Graphviz rendering) and INflow (interactive web app). The
format is defined by that repository's @schema-net.json@: a top-level object
with an @agents@ array and an @edges@ array.

Each net node becomes an /agent/: its @id@ is the node number, its @label@ a
short human-readable tag, and its ports are split into a single
@principalPort@ (the 'principalPort' of the 'INet' instance) and the remaining
@auxiliaryPorts@. Port ids are local to the agent: the principal port is
@P0@ and the auxiliary ports are @P1@, @P2@, ... in the order given by the
'View' instance. Each net edge (a pair of connected ports) becomes an /edge/
connecting the two agents' ports.

'dumpReduction' is the entry point for visual debugging: it reduces a net
step by step ('Lamagraph.Ski.YaleReduce.reduceStepsYaleTrace') and writes one
JSON file per step (@step_0000.json@, @step_0001.json@, ...) into a directory,
ready to be rendered as a sequence of pictures by INviz.
-}
module Lamagraph.Ski.YaleDraw (
  netToInviz,
  netToInvizText,
  dumpNet,
  dumpReduction,
) where

import Relude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.View (View (inspect))
import GraphRewriting (
  Edge,
  Graph,
  Node,
  Rewrite,
  evalGraph,
  readNode,
  readNodeList,
 )
import GraphRewriting.Pattern.InteractionNet (INet (principalPort))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Lamagraph.Ski.YaleNet (YaleNode (..))
import Lamagraph.Ski.YaleReduce (reduceStepsYaleTrace)

-- * Net -> INviz JSON

{- | Convert a YALE net to the INviz JSON value (see the module docs for the
mapping). Deterministic: agents are ordered by node number and edges by their
endpoint (agent, port) pairs.
-}
netToInviz :: Graph YaleNode -> Aeson.Value
netToInviz = evalGraph collect
 where
  collect :: Rewrite YaleNode Aeson.Value
  collect = do
    ns <- readNodeList
    infos <- mapM (\n -> (, n) <$> readNode n) ns
    let sorted = sortBy (compare `on` snd) infos
        -- (agentId, label, [(portId, edge)]) per node
        nodeInfos :: [(String, String, [(String, Edge)])]
        nodeInfos = [(agentId n, labelOf d, portIds d) | (d, n) <- sorted]
        -- edge -> [(agentId, portId)] (the two endpoints of the edge)
        edgeMap :: Map.Map Edge [(String, String)]
        edgeMap = foldl' addEndpoints Map.empty nodeInfos
        addEndpoints m (aid, _, pids) =
          foldl' (\m2 (pid, e) -> Map.insertWith (++) e [(aid, pid)] m2) m pids
        agents :: [Aeson.Value]
        agents = [agentObj aid lbl pids | (aid, lbl, pids) <- nodeInfos]
        edges :: [Aeson.Value]
        edges =
          [ edgeObj a1 p1 a2 p2
              | (_, eps) <- Map.toAscList edgeMap
              , let seps = sortBy (compare `on` (\(a, p) -> (a, p))) eps
              , [(a1, p1), (a2, p2)] <- [seps]
          ]
    return (Aeson.object ["agents" Aeson..= agents, "edges" Aeson..= edges])

-- | The INviz JSON for a net, as UTF-8 text (compact encoding).
netToInvizText :: Graph YaleNode -> Text
netToInvizText g = decodeUtf8 (Aeson.encode (netToInviz g))

-- * Dumping to files

{- | Write a single net to @path@ as INviz JSON.
-}
dumpNet :: FilePath -> Graph YaleNode -> IO ()
dumpNet path g = BSL.writeFile path (Aeson.encode (netToInviz g))

{- | Reduce a net for at most @maxSteps@ steps and write one INviz JSON file
per step into @dir@ (created if missing): @step_0000.json@ is the net after
the compile-time binders are compiled away (before any interaction),
@step_0001.json@ after the first interaction, and so on. The final file is the
normal form (or the net at the step budget). Feed @dir@ to INviz
(@python3 INviz/main.py dir@) to render the reduction as a picture sequence.
-}
dumpReduction :: FilePath -> Int -> Graph YaleNode -> IO ()
dumpReduction dir maxSteps g0 = do
  createDirectoryIfMissing True dir
  let graphs = reduceStepsYaleTrace maxSteps g0
  mapM_
    ( \(i, g) -> BSL.writeFile (dir </> stepName i) (Aeson.encode (netToInviz g)) )
    (zip [0 :: Int ..] graphs)
 where
  stepName :: Int -> FilePath
  stepName i = "step_" <> pad4 (show i) <> ".json"
  pad4 :: String -> String
  pad4 s = replicate (max 0 (4 - length s)) '0' <> s

-- * Node -> agent helpers

-- | The agent id for a node: @N@ followed by the node number.
agentId :: Node -> String
agentId n = "N" <> show n

-- | A short human-readable label for a node.
labelOf :: YaleNode -> String
labelOf n = case n of
  Initiator{} -> "Init"
  YaleLam{} -> "Lam"
  YaleApp{} -> "App"
  Copy{} -> "Copy"
  Subst{} -> "Subst"
  Erase{} -> "Erase"
  FreeVar{name = nm} -> "FreeVar:" <> nm
  FreeVarBlocked{name = nm} -> "FreeVarBlocked:" <> nm
  EndList{} -> "End"
  Garbage{} -> "Garbage"
  Forcing{} -> "Forcing"
  Constant{name = nm} -> nm
  Multiplexer{} -> "Mux"

{- | The ports of a node as @(portId, edge)@ pairs: the principal port is
@P0@ and the auxiliary ports are @P1@, @P2@, ... in the order given by the
'View' instance.
-}
portIds :: YaleNode -> [(String, Edge)]
portIds d =
  let pp = principalPort d
      auxs = filter (/= pp) (inspect d)
      auxIds = zip (map (\i -> "P" <> show (i + 1)) [0 :: Int ..]) auxs
   in ("P0", pp) : auxIds

-- | An agent JSON object. @pids@ is @(principalPort : auxiliaryPorts)@ (the
-- principal port is always first, see 'portIds').
agentObj :: String -> String -> [(String, Edge)] -> Aeson.Value
agentObj aid lbl ((pid0, _) : auxs) =
  Aeson.object
    [ "id" Aeson..= aid
    , "label" Aeson..= lbl
    , "principalPort" Aeson..= portObj pid0
    , "auxiliaryPorts" Aeson..= map (portObj . fst) auxs
    ]
-- An agent always has at least its principal port, so this is unreachable; it
-- exists only to make the pattern above exhaustive.
agentObj aid lbl [] =
  Aeson.object
    [ "id" Aeson..= aid
    , "label" Aeson..= lbl
    , "principalPort" Aeson..= portObj "P0"
    , "auxiliaryPorts" Aeson..= ([] :: [Aeson.Value])
    ]

-- | A port JSON object (the label is left empty).
portObj :: String -> Aeson.Value
portObj pid =
  let lbl :: String
      lbl = ""
   in Aeson.object ["id" Aeson..= pid, "label" Aeson..= lbl]

-- | An edge JSON object.
edgeObj :: String -> String -> String -> String -> Aeson.Value
edgeObj a1 p1 a2 p2 =
  Aeson.object
    [ "id" Aeson..= ("E_" <> a1 <> ":" <> p1 <> "-" <> a2 <> ":" <> p2)
    , "source" Aeson..= a1
    , "target" Aeson..= a2
    , "sourcePort" Aeson..= p1
    , "targetPort" Aeson..= p2
    , "activePair" Aeson..= (False :: Bool)
    ]
