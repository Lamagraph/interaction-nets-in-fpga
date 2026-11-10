{- | Decoding for a normalized YALE (Mackie 1998) lambda interaction net.

The shape-directed decoder 'skiEvalYale' mirrors the SKI decoder
('Lamagraph.Ski.Decode.decodeValue'): it is directed by the shape (the LML
interpreter's result), which supplies the type information the YALE normal form
lacks. For each shape, a fresh YALE net is built (the result term with the
appropriate decoder applied), reduced to normal form, and the value is
extracted from the graph.

Church numerals are decoded by applying the result to a tick-counting decoder
and a base, then counting the surviving tick markers. Booleans are decoded by
applying the result to two distinct markers and reading which survives at the
root. Tuples and ADTs are decoded by projecting fields with a fresh net per
projection.

The low-level helpers 'rootConstantNameYale' and 'countMarkersYale' are
exported for testing.
-}
module Lamagraph.Ski.YaleDecode (
  rootConstantNameYale,
  countMarkersYale,
  skiEvalYale,
) where

import Relude

import Data.Set qualified as Set
import Data.Text qualified as T
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

import Lamagraph.Compiler.Core (CoreBind, CoreExpr)
import Lamagraph.Compiler.Typechecker.TcTypes (Name)
import Lamagraph.Ski.Decode (baseName, tickName)
import Lamagraph.Ski.Error (SkiError (..))
import Lamagraph.Ski.Lambda (Lam (..))
import Lamagraph.Ski.Run (MaxSteps (..), StepOverflow (..), defaultMaxSteps)
import Lamagraph.Ski.Translate (translateLam)
import Lamagraph.Ski.Value (Value' (..))
import Lamagraph.Ski.YaleNet (YaleNode (..), buildYaleNet)
import Lamagraph.Ski.YaleReduce (reduceStepsYale)

-- * Marker names

-- | Marker name for the @true@ branch of a Church-boolean decode.
tickTName :: String
tickTName = "__tickT__"

-- | Marker name for the @false@ branch of a Church-boolean decode.
tickFName :: String
tickFName = "__tickF__"

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

-- * Build and reduce

{- | Build a YALE net for a 'Lam' term and reduce it to normal form within the
step budget. Returns the reduced graph, or 'Left' with the step count at which
the budget was exhausted.
-}
buildAndReduce :: MaxSteps -> Lam -> Either SkiError (Graph YaleNode)
buildAndReduce (MaxSteps n) lam =
  let g = buildYaleNet lam
      (steps, g2, nf) = reduceStepsYale n g
   in if nf
        then Right g2
        else Left (StepOverflowReached (StepOverflow steps))

-- * Shape-directed decode

{- | Decode a YALE-normalized result to a canonical value, directed by the
shape (the LML interpreter's result). Mirrors
'Lamagraph.Ski.Decode.decodeValue': the shape supplies the type information the
YALE normal form lacks; for each shape, a fresh YALE net is built (the result
term with the appropriate decoder applied), reduced, and the value is extracted.
-}
decodeYaleValue :: MaxSteps -> Value' -> Lam -> Either SkiError Value'
decodeYaleValue maxSteps shape lam = case shape of
  RInt _ -> decodeYaleChurch maxSteps lam <&> RInt
  RBool _ -> decodeYaleBool maxSteps lam <&> RBool
  RChar _ -> decodeYaleChurch maxSteps lam <&> \n -> RChar (chr n)
  RString s -> decodeYaleString maxSteps s lam
  RTuple vs -> decodeYaleTuple maxSteps vs lam
  RAdt con vs -> decodeYaleAdt maxSteps con vs lam

-- | Count the 'YaleApp' nodes in a reduced YALE net.
countAppsYale :: Graph YaleNode -> Int
countAppsYale = evalGraph go
 where
  go = do
    ns <- readNodeList
    mapM readNode ns
      <&> length
      . filter
        ( \case
            YaleApp{} -> True
            _ -> False
        )

{- | Decode a Church-encoded integer: apply the result to the @tick@ constant
and the @base@ constant directly (no intermediate lambda, avoiding the
unused-argument problem). The result is a chain of @n@ 'YaleApp' nodes
(@tick^n(base)@); count them.
-}
decodeYaleChurch :: MaxSteps -> Lam -> Either SkiError Int
decodeYaleChurch maxSteps lam = do
  let full = LApp (LApp lam (LVar tickName)) (LVar baseName)
  g <- buildAndReduce maxSteps full
  pure (countAppsYale g)

{- | Decode a Church-encoded boolean: apply the result to two distinct markers;
whichever survives at the root indicates the value.
-}
decodeYaleBool :: MaxSteps -> Lam -> Either SkiError Bool
decodeYaleBool maxSteps lam = do
  let full = LApp (LApp lam (LVar tickTName)) (LVar tickFName)
  g <- buildAndReduce maxSteps full
  case rootConstantNameYale g of
    Just nm -> pure (nm == tickTName)
    Nothing -> Left (StepOverflowReached (StepOverflow 0))

-- | The Church pair first projection: @λa b. a@.
fstSelLam :: Lam
fstSelLam = LLam "__a__" (LLam "__b__" (LVar "__a__"))

-- | The Church pair second projection: @λa b. b@.
sndSelLam :: Lam
sndSelLam = LLam "__a__" (LLam "__b__" (LVar "__b__"))

{- | Decode a nested-Church-pair term to its elements, directed by the shape
(the expected list of element shapes). Each projection is a fresh net.
-}
decodeYalePairFields :: MaxSteps -> [Value'] -> Lam -> Either SkiError [Value']
decodeYalePairFields _ [] _ = Left (StepOverflowReached (StepOverflow 0))
decodeYalePairFields maxSteps [x] lam = decodeYaleValue maxSteps x lam <&> (: [])
decodeYalePairFields maxSteps (x : xs) lam = do
  v1 <- decodeYaleValue maxSteps x (LApp lam fstSelLam)
  rest <- decodeYalePairFields maxSteps xs (LApp lam sndSelLam)
  pure (v1 : rest)

-- | Decode a nested-Church-pair term to a tuple, directed by the shape.
decodeYaleTuple :: MaxSteps -> [Value'] -> Lam -> Either SkiError Value'
decodeYaleTuple maxSteps shapes lam = RTuple <$> decodeYalePairFields maxSteps shapes lam

{- | Decode a tagged-ADT term to an ADT value, directed by the shape (the
constructor name and the expected field shapes). The tag is supplied by the
shape; only the fields are projected.
-}
decodeYaleAdt :: MaxSteps -> Name -> [Value'] -> Lam -> Either SkiError Value'
decodeYaleAdt _ con [] _ = pure (RAdt con [])
decodeYaleAdt maxSteps con fieldShapes lam =
  RAdt con <$> decodeYalePairFields maxSteps fieldShapes (LApp lam sndSelLam)

{- | Decode a string term to a string, directed by the shape (the expected
string, which supplies the length).
-}
decodeYaleString :: MaxSteps -> Text -> Lam -> Either SkiError Value'
decodeYaleString maxSteps s lam
  | T.null s = pure (RString T.empty)
  | otherwise = case decodeYaleTuple maxSteps [RInt 0 | _ <- T.unpack s] lam of
      Left e -> Left e
      Right (RTuple elems) ->
        let codePoints = [n | RInt n <- elems]
         in if length codePoints == T.length s
              then pure (RString (T.pack (map chr codePoints)))
              else Left (StepOverflowReached (StepOverflow 0))
      Right _ -> Left (StepOverflowReached (StepOverflow 0))

{- | Evaluate a closed LML core program to a canonical value via the YALE
pipeline: translate to a 'Lam' term, then decode the result (directed by the
shape, the LML interpreter's result) with the default step budget. Fails with
'UnsupportedConstruct' if the program uses a construct outside the supported
subset, or 'StepOverflowReached' if the reduction exhausts the budget.
-}
skiEvalYale :: Value' -> [CoreBind] -> CoreExpr -> Either SkiError Value'
skiEvalYale shape binds resultExpr = do
  lam <- translateLam binds resultExpr
  decodeYaleValue defaultMaxSteps shape lam
