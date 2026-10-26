{- | A worklist-driven reduction driver for SKI interaction nets.

The fixed-order driver ('Lamagraph.Ski.Run.reduceStepsFixedOrder') rescans the
entire graph for every rule on every step, so each step costs O(graph size).
This driver instead tracks the set of redex pairs and, after each contraction,
only re-examines the local neighbourhood of the contracted pair, turning the
per-step cost into O(local degree). That is the dominant factor for the verbose
Church-encoded terms.

Correctness is guaranteed by a safety net: whenever the worklist drains, a full
scan of the graph is performed, and normal form is declared only when that full
scan finds no redex. The driver is therefore correct even if the incremental
neighbourhood update ever misses a pair.
-}
module Lamagraph.Ski.Worklist (
  reduceStepsWL,
) where

import Relude

import Control.Monad (foldM)
import Data.Set qualified as Set
import GraphRewriting (
  Graph,
  Node,
  Rewrite,
  attachedNodes,
  deleteNode,
  evalGraph,
  existNode,
  mergeEdges,
  neighbours,
  newEdge,
  newNode,
  readEdgeList,
  readNode,
  runGraph,
 )
import GraphRewriting.Pattern.InteractionNet (principalPort)
import INet.Graph (SKI (..))

-- * Driver

{- | Reduce a SKI graph for at most @n@ steps using the worklist driver. Returns
the step count reached, the resulting graph, and whether normal form was reached
(no redex remains, confirmed by a full scan).
-}
reduceStepsWL :: Int -> Graph SKI -> (Int, Graph SKI, Bool)
reduceStepsWL maxSteps g0 = go 0 g0 (initialWorklist g0) maxSteps
 where
  go step g wl remaining
    | remaining <= 0 = (step, g, False)
    | Set.null wl =
        let wl' = initialWorklist g
         in if Set.null wl'
              then (step, g, True)
              else go step g wl' remaining
    | otherwise =
        case Set.minView wl of
          Nothing -> go step g Set.empty remaining
          Just ((u, v), wl') ->
            case runGraph (tryContract u v) g of
              (Nothing, _) -> go step g wl' remaining
              (Just affected, g') ->
                let wl'' = addConnectedPairs wl' g' affected
                 in go (step + 1) g' wl'' (remaining - 1)

-- * Worklist construction

-- | Normalise an unordered node pair so that the smaller key comes first.
normPair :: Node -> Node -> (Node, Node)
normPair a b = if a <= b then (a, b) else (b, a)

{- | Whether the two node values form a redex: an active pair (shared principal
port) matching some SKI rule, or an eraser / duplicator pair for the
@eliminate@ rule. Crucially this is stricter than merely sharing an edge — a
shared principal port with no matching rule is a dead pair that never
contracts, and admitting such pairs would make the driver loop forever.
-}
isRedex :: SKI -> SKI -> Bool
isRedex su sv = isJust (classifyActive su sv) || isJust (classifyElim su sv)

{- | The initial set of redex pairs: a full scan of the graph. Used to seed the
worklist and as the safety net that confirms normal form (an empty result means
no rule can fire).
-}
initialWorklist :: Graph SKI -> Set (Node, Node)
initialWorklist = evalGraph go
 where
  go = foldM addEdge Set.empty =<< readEdgeList
  addEdge acc e = do
    ns <- attachedNodes e
    vs <- mapM readNode ns
    let cands =
          [ normPair u v
          | (u, su) <- zip ns vs
          , (v, sv) <- zip ns vs
          , u < v
          , isRedex su sv
          ]
    return (foldr Set.insert acc cands)

{- | Add, for each affected node, the redex pairs it forms with its neighbours
in the new graph. Only genuine redexes are admitted (via 'isRedex'); adding
every connected pair would flood the worklist with dead pairs.
-}
addConnectedPairs :: Set (Node, Node) -> Graph SKI -> [Node] -> Set (Node, Node)
addConnectedPairs wl g ns = evalGraph (go wl) g
 where
  go acc =
    foldM
      ( \a n -> do
          su <- readNode n
          ms <- neighbours n
          foldM
            ( \a2 m -> do
                sv <- readNode m
                return (if isRedex su sv then Set.insert (normPair n m) a2 else a2)
            )
            a
            ms
      )
      acc
      ns

-- * Local rule application

-- | The SKI rule that fires for a candidate pair.
data RuleKind
  = RK_I
  | RK_K0
  | RK_K1
  | RK_S0
  | RK_S1
  | RK_S2
  | RK_E0
  | RK_E1
  | RK_E2
  | RK_D0
  | RK_D1
  | RK_D2
  | RK_Elim
  deriving (Show, Eq)

{- | Attempt to contract the redex formed by nodes @u@ and @v@. Returns 'Just'
with the affected nodes (for the worklist update) if a rule fired, or 'Nothing'
if the pair is not a redex (leaving the graph unchanged).
-}
tryContract :: Node -> Node -> Rewrite SKI (Maybe [Node])
tryContract u v = do
  eu <- existNode u
  ev <- existNode v
  if not (eu && ev)
    then return Nothing
    else do
      su <- readNode u
      sv <- readNode v
      case classifyActive su sv of
        Just (kind, xv, yv) -> fmap Just (contractActive kind u v xv yv)
        Nothing -> case classifyElim su sv of
          Just (ev', dv) -> fmap Just (contractElim u v ev' dv)
          Nothing -> return Nothing

{- | Classify an active pair (shared principal port) into a rule kind, returning
the oriented (first, second) node values.
-}
classifyActive :: SKI -> SKI -> Maybe (RuleKind, SKI, SKI)
classifyActive su sv
  | principalPort su == principalPort sv = matchFirst su sv <|> matchFirst sv su
  | otherwise = Nothing
 where
  matchFirst x y = case (x, y) of
    (I{}, A{}) -> Just (RK_I, x, y)
    (K0{}, A{}) -> Just (RK_K0, x, y)
    (K1{}, A{}) -> Just (RK_K1, x, y)
    (S0{}, A{}) -> Just (RK_S0, x, y)
    (S1{}, A{}) -> Just (RK_S1, x, y)
    (S2{}, A{}) -> Just (RK_S2, x, y)
    (E{}, I{}) -> Just (RK_E0, x, y)
    (E{}, K0{}) -> Just (RK_E0, x, y)
    (E{}, S0{}) -> Just (RK_E0, x, y)
    (E{}, K1{}) -> Just (RK_E1, x, y)
    (E{}, S1{}) -> Just (RK_E1, x, y)
    (E{}, S2{}) -> Just (RK_E2, x, y)
    (D{}, I{}) -> Just (RK_D0, x, y)
    (D{}, K0{}) -> Just (RK_D0, x, y)
    (D{}, S0{}) -> Just (RK_D0, x, y)
    (D{}, K1{}) -> Just (RK_D1, x, y)
    (D{}, S1{}) -> Just (RK_D1, x, y)
    (D{}, S2{}) -> Just (RK_D2, x, y)
    _ -> Nothing

{- | Classify an eraser / duplicator pair for the @eliminate@ rule, returning the
oriented (eraser, duplicator) values. The eraser's input port must coincide with
one of the duplicator's input ports.
-}
classifyElim :: SKI -> SKI -> Maybe (SKI, SKI)
classifyElim su sv =
  case (su, sv) of
    (E{inp = iE}, D{inp1 = i1, inp2 = i2})
      | iE == i1 || iE == i2 -> Just (su, sv)
    (D{inp1 = i1, inp2 = i2}, E{inp = iE})
      | iE == i1 || iE == i2 -> Just (sv, su)
    _ -> Nothing

-- | Perform an active-pair contraction, returning the affected nodes.
contractActive :: RuleKind -> Node -> Node -> SKI -> SKI -> Rewrite SKI [Node]
contractActive kind u v xv yv = case (kind, xv, yv) of
  (RK_I, I{}, A{inp = iA, out2 = o2}) -> do
    aff0 <- neighBoth u v
    mergeEdges iA o2
    deleteNode u
    deleteNode v
    return aff0
  (RK_K0, K0{}, A{inp = iA, out2 = o2}) -> do
    aff0 <- neighBoth u v
    n <- newNode (K1{inp = iA, out = o2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_K1, K1{out = oK}, A{inp = iA, out2 = o2A}) -> do
    aff0 <- neighBoth u v
    n <- newNode (E{inp = o2A})
    mergeEdges iA oK
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_S0, S0{}, A{inp = iA, out2 = o2A}) -> do
    aff0 <- neighBoth u v
    n <- newNode (S1{inp = iA, out = o2A})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_S1, S1{out = oS}, A{inp = iA, out2 = o2A}) -> do
    aff0 <- neighBoth u v
    n <- newNode (S2{inp = iA, out1 = oS, out2 = o2A})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_S2, S2{inp = iS, out1 = oS1, out2 = o2S}, A{inp = iA, out1 = o1A, out2 = o2A}) -> do
    aff0 <- neighBoth u v
    i1D <- newEdge
    iB <- newEdge
    i2D <- newEdge
    n1 <- newNode (A{inp = iS, out1 = oS1, out2 = i1D})
    n2 <- newNode (A{inp = iA, out1 = o1A, out2 = iB})
    n3 <- newNode (D{inp1 = i1D, inp2 = i2D, out = o2A})
    n4 <- newNode (A{inp = iB, out1 = o2S, out2 = i2D})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2, n3, n4])
  (RK_E0, E{}, _) -> do
    aff0 <- neighBoth u v
    deleteNode u
    deleteNode v
    return aff0
  (RK_E1, E{}, K1{out = oN}) -> do
    aff0 <- neighBoth u v
    n <- newNode (E{inp = oN})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_E1, E{}, S1{out = oN}) -> do
    aff0 <- neighBoth u v
    n <- newNode (E{inp = oN})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n])
  (RK_E2, E{}, S2{out1 = o1, out2 = o2}) -> do
    aff0 <- neighBoth u v
    n1 <- newNode (E{inp = o1})
    n2 <- newNode (E{inp = o2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2])
  (RK_D0, D{inp1 = iD1, inp2 = iD2}, I{}) -> do
    aff0 <- neighBoth u v
    n1 <- newNode (I{inp = iD1})
    n2 <- newNode (I{inp = iD2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2])
  (RK_D0, D{inp1 = iD1, inp2 = iD2}, K0{}) -> do
    aff0 <- neighBoth u v
    n1 <- newNode (K0{inp = iD1})
    n2 <- newNode (K0{inp = iD2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2])
  (RK_D0, D{inp1 = iD1, inp2 = iD2}, S0{}) -> do
    aff0 <- neighBoth u v
    n1 <- newNode (S0{inp = iD1})
    n2 <- newNode (S0{inp = iD2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2])
  (RK_D1, D{inp1 = iD1, inp2 = iD2}, K1{out = oN}) -> do
    aff0 <- neighBoth u v
    iD1' <- newEdge
    iD2' <- newEdge
    n1 <- newNode (K1{inp = iD1, out = iD1'})
    n2 <- newNode (K1{inp = iD2, out = iD2'})
    n3 <- newNode (D{inp1 = iD1', inp2 = iD2', out = oN})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2, n3])
  (RK_D1, D{inp1 = iD1, inp2 = iD2}, S1{out = oN}) -> do
    aff0 <- neighBoth u v
    iD1' <- newEdge
    iD2' <- newEdge
    n1 <- newNode (S1{inp = iD1, out = iD1'})
    n2 <- newNode (S1{inp = iD2, out = iD2'})
    n3 <- newNode (D{inp1 = iD1', inp2 = iD2', out = oN})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2, n3])
  (RK_D2, D{inp1 = iD1, inp2 = iD2}, S2{out1 = o1, out2 = o2}) -> do
    aff0 <- neighBoth u v
    l1 <- newEdge
    l2 <- newEdge
    x1 <- newEdge
    x2 <- newEdge
    n1 <- newNode (S2{inp = iD1, out1 = l1, out2 = x1})
    n2 <- newNode (S2{inp = iD2, out1 = x2, out2 = l2})
    n3 <- newNode (D{inp1 = l1, inp2 = x2, out = o1})
    n4 <- newNode (D{inp1 = x1, inp2 = l2, out = o2})
    deleteNode u
    deleteNode v
    return (aff0 ++ [n1, n2, n3, n4])
  _ -> error "contractActive: unclassified rule kind"

-- | Perform the @eliminate@ contraction, returning the affected nodes.
contractElim :: Node -> Node -> SKI -> SKI -> Rewrite SKI [Node]
contractElim u v E{inp = iE} D{out = oD, inp1 = i1, inp2 = i2}
  | iE == i1 = do
      aff0 <- neighBoth u v
      mergeEdges oD i2
      deleteNode u
      deleteNode v
      return aff0
  | iE == i2 = do
      aff0 <- neighBoth u v
      mergeEdges oD i1
      deleteNode u
      deleteNode v
      return aff0
  | otherwise = error "contractElim: eraser port does not match a duplicator input"
contractElim _ _ _ _ = error "contractElim: expected an eraser and a duplicator"

{- | The neighbours of both nodes (before deletion), for the worklist update.
Excludes @u@ and @v@ themselves: @neighbours u@ contains @v@ and vice versa, and
both are deleted by the contraction, so they must not be read in the new graph.
-}
neighBoth :: Node -> Node -> Rewrite SKI [Node]
neighBoth u v = do
  nu <- neighbours u
  nv <- neighbours v
  return [n | n <- nu ++ nv, n /= u && n /= v]
