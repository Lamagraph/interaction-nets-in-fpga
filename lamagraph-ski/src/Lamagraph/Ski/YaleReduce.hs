{- | A step-bounded reduction driver for the full YALE (Mackie 1998) lambda
interaction net.

The interactions (all principal-port meetings), with their source in the paper:

* β-reduction (Mackie §5, first rule) — a 'YaleApp' met at its principal
  (function) port by a 'YaleLam' met at its principal (result) port: both are
  deleted, the abstraction's body is wired to the application's result, the
  application's argument is wired to the abstraction's bound-variable port
  (@lamVar@), and an 'Erase' (@d@) agent is introduced at the head of the
  abstraction's free-variable list.
* 'Erase'×'EndList' (Mackie §5, second rule) — the @d@ agent erases the
  end-of-list @ν@ (the list is empty; the erasure is complete).
* 'Copy'×'YaleLam' (Mackie §5, "Duplication") — the @c@ agent copies the
  abstraction: two copies of the @λ@ are produced, a 'Subst' (@s@) agent is
  propagated into each copy's body, and a 'Copy' is propagated along each
  copy's free-variable list.
* 'Copy'×'YaleApp' (Mackie §5, "Duplication") — the @c@ agent copies the
  application: two copies of the @@@ are produced, and a 'Copy' is propagated
  along each copy's auxiliary ports.
* 'Copy'×'Constant' (Mackie §5, "Duplication") — the @c@ agent copies a base
  value: two copies.
* 'Copy'×'EndList' (Mackie §5, "Duplication") — the @c@ agent copies the
  end-of-list: two @ν@'s.
* 'Subst'×'YaleApp' (Mackie §5, "Substitution") — the @s@ agent propagates
  into the application's function and argument.
* 'Subst'×'YaleLam' (Mackie §5, "Substitution") — the @s@ agent propagates
  into the abstraction's body.
* 'Subst'×'Constant' (Mackie §5, "Substitution") — the @s@ agent is erased at
  a base value.
* 'Garbage'×(any term) (Mackie §5) — the @e@ agent erases the term and the
  net on its auxiliary ports.

Recursion is encoded as a cyclic net ("tie the knot", Mackie §6): two
'YaleApp' nodes, each fed by its own copy of @F = λself. inner@; the bottom
app's result is the knot edge, which is also its argument. The β-reduction
splices the knot edge onto the bound variable, yielding the self-reference.

The driver is a worklist: at each step it picks one active pair (two
interacting nodes joined at their principal ports) and rewrites it, until no
active pair remains (normal form) or the step budget is exhausted. The firing
order is arbitrary (the first active pair in node order).
-}
module Lamagraph.Ski.YaleReduce (
  reduceStepsYale,
  reduceStepsYaleTrace,
  reduceStepsYaleTags,
) where

import Relude

import GraphRewriting (
  Edge,
  Graph,
  Node,
  Replace,
  Rewrite,
  Rule,
  apply',
  attachedNodes,
  byConnector,
  byEdge,
  byNode,
  deleteEdge,
  deleteNode,
  evalGraph,
  inspectNode,
  mergeEdges,
  newEdge,
  newNode,
  nodeAt,
  readNode,
  readNodeList,
  replace,
  require,
  rewrite,
  runGraph,
 )
import GraphRewriting.Pattern.InteractionNet (INet (..))

import Lamagraph.Ski.YaleNet (YaleNode (..), compileBinders)

-- * Driver

{- | Reduce a YALE lambda interaction net for at most @n@ steps. The
compile-time 'Multiplexer' binders are first compiled away by
'Lamagraph.Ski.YaleNet.compileBinders' (not counted as a step). The reduction
then fires interactions in a worklist: at each step it picks one active pair
(two interacting nodes joined at their principal ports) and rewrites it, until
no active pair remains (normal form) or the step budget is exhausted.

Returns the step count, the resulting graph, and whether normal form was
reached.
-}
reduceStepsYale :: Int -> Graph YaleNode -> (Int, Graph YaleNode, Bool)
reduceStepsYale maxSteps g0 =
  let g1 = compileBinders g0
   in go 0 g1 maxSteps
 where
  go step g remaining
    | remaining <= 0 = (step, g, False)
    | otherwise = case findActivePairs g of
        [] -> (step, g, True)
        (a, b) : _ -> go (step + 1) (fireInteraction g a b) (remaining - 1)

-- TEMPORARY DEBUG: trace the tag pair of each fired interaction and dump the net.
reduceStepsYaleTrace :: Int -> Graph YaleNode -> IO (Int, Graph YaleNode, Bool)
reduceStepsYaleTrace maxSteps g0 = do
  let g1 = compileBinders g0
  go 0 g1 maxSteps
 where
  go step g remaining
    | remaining <= 0 = return (step, g, False)
    | otherwise = case findActivePairs g of
        [] -> return (step, g, True)
        (a, b) : _ ->
          let tags = readTags g a b
           in do
                putStrLn $ "  step " <> show step <> ": " <> show tags
                dumpNet g
                go (step + 1) (fireInteraction g a b) (remaining - 1)

-- TEMPORARY DEBUG: print only the tag pair of each fired interaction (no net dump).
reduceStepsYaleTags :: Int -> Graph YaleNode -> IO (Int, Graph YaleNode, Bool)
reduceStepsYaleTags maxSteps g0 = do
  let g1 = compileBinders g0
  go 0 g1 maxSteps
 where
  go step g remaining
    | remaining <= 0 = return (step, g, False)
    | otherwise = case findActivePairs g of
        [] -> return (step, g, True)
        (a, b) : _ ->
          let tags = readTags g a b
           in do
                putStrLn $ "  step " <> show step <> ": " <> show tags
                go (step + 1) (fireInteraction g a b) (remaining - 1)

dumpNet :: Graph YaleNode -> IO ()
dumpNet g =
  let readPair n = do
        v <- readNode n
        return (n, v)
      m = do
        ns <- readNodeList
        mapM readPair ns
   in mapM_ (\(n, v) -> putStrLn $ "      n" <> show n <> ": " <> show v) (evalGraph m g)

readTags :: Graph YaleNode -> Node -> Node -> (Tag, Tag)
readTags g a b =
  let m = do
        x <- readNode a
        y <- readNode b
        return (tag x, tag y)
   in runReader m g

{- | All active pairs of the net: unordered pairs of distinct interacting nodes
joined at their principal (port 0) edge, where the pair actually interacts.
The order is deterministic: pairs involving a 'Copy' agent come first (so that
duplication is performed before β-reduction can consume the term being copied),
then remaining pairs in node order.
-}
findActivePairs :: Graph YaleNode -> [(Node, Node)]
findActivePairs = runReader go
 where
  go :: Reader (Graph YaleNode) [(Node, Node)]
  go = do
    ns <- readNodeList
    vs <- mapM readNode ns
    let tagged = [(a, va) | (a, va) <- zip ns vs, interacting va]
    let pairs =
          [ (a, b)
          | (a, va) <- tagged
          , (b, vb) <- tagged
          , b > a
          , canMeet va vb
          , interacts va vb
          ]
    -- Prioritize pairs involving a Copy agent: duplication must happen before
    -- β-reduction can consume the term being copied.
    let copyNodes = [a | (a, va) <- tagged, isCopy va]
    let isCopyPair (a, b) = a `elem` copyNodes || b `elem` copyNodes
    let copyPairs = filter isCopyPair pairs
    let otherPairs = filter (not . isCopyPair) pairs
    return (copyPairs ++ otherPairs)
   where
    isCopy (Copy{}) = True
    isCopy _ = False

{- | Whether two agents can meet: they share an edge and at least one of them
 is on its principal port (standard interaction-net semantics, Mackie 1998).
 The common case is that both are on their principal ports. There is one
 deliberate exception: a 'Copy' (@c@) or 'Subst' (@s@) agent may meet a
 'YaleApp' (@@@) at the application's result port, which is an auxiliary port
 of the @@@. This is how the body-copy and body-substitution agents introduced
 by 'copyLamRHS' process an application body: they sit on the abstraction's
 body edge, which is exactly the body application's result port, while the @@@'s
 principal is its function port. Under a strict "both on their principal" rule
 the two could never meet and the duplication would deadlock.
-}
canMeet :: YaleNode -> YaleNode -> Bool
canMeet va vb =
  principalPort va == principalPort vb
    || agentAtAppResult va vb
    || agentAtAppResult vb va
 where
  agentAtAppResult a@(Copy{}) (YaleApp{appResult = r}) = principalPort a == r
  agentAtAppResult a@(Subst{}) (YaleApp{appResult = r}) = principalPort a == r
  agentAtAppResult _ _ = False

-- * Firing a specific interaction

{- | Fire the interaction between two specific nodes @a@ and @b@ that are
connected at their principal (port 0) edge. Returns the rewritten graph
(unchanged if the pair does not interact).
-}
fireInteraction :: Graph YaleNode -> Node -> Node -> Graph YaleNode
fireInteraction g a b =
  let (fired, g2) = runGraph (apply' (fireAt a b)) g
   in if fired
        then g2
        else g

-- | A rule that fires the interaction on the specific nodes @a@ and @b@.
fireAt :: Node -> Node -> Rule YaleNode
fireAt a b = do
  va <- nodeAt a
  vb <- nodeAt b
  require (interacting va && interacting vb)
  require (a /= b)
  require (canMeet va vb)
  case (tag va, tag vb) of
    -- β-reduction: @ × λ (uses 'rewrite' so the RHS can call 'deleteEdge')
    (TApp, TLam) -> rewrite $ \_ -> betaRHS va vb a b
    (TLam, TApp) -> rewrite $ \_ -> betaRHS vb va b a
    _ ->
      replace $ case (tag va, tag vb) of
        -- Copy: c × X
        (TCopy, TCopy) -> return ()
        (TCopy, TLam) -> copyLamRHS va vb
        (TLam, TCopy) -> copyLamRHS vb va
        (TCopy, TApp) -> copyAppRHS va vb
        (TApp, TCopy) -> copyAppRHS vb va
        (TCopy, TConst) -> copyConstRHS va vb
        (TConst, TCopy) -> copyConstRHS vb va
        (TCopy, TEnd) -> copyEndRHS va
        (TEnd, TCopy) -> copyEndRHS vb
        (TCopy, TFreeV) -> copyFreeVarRHS va vb
        (TFreeV, TCopy) -> copyFreeVarRHS vb va
        (TCopy, TFVB) -> copyFreeVarBlockedRHS va vb
        (TFVB, TCopy) -> copyFreeVarBlockedRHS vb va
        -- Substitution: s × X
        (TSubst, TApp) -> substAppRHS va vb
        (TApp, TSubst) -> substAppRHS vb va
        (TSubst, TLam) -> substLamRHS va vb
        (TLam, TSubst) -> substLamRHS vb va
        (TSubst, TConst) -> substConstRHS va vb
        (TConst, TSubst) -> substConstRHS vb va
        (TSubst, TBV) -> substBoundVarRHS va vb
        (TBV, TSubst) -> substBoundVarRHS vb va
        -- Erase: d × X
        (TErase, TEnd) -> eraseRHS
        (TEnd, TErase) -> eraseRHS
        (TErase, TFreeV) -> eraseRHS
        (TFreeV, TErase) -> eraseRHS
        (TErase, TFVB) -> eraseRHS
        (TFVB, TErase) -> eraseRHS
        -- Garbage: e × X
        (TGarbage, _) -> garbageRHS va vb
        (_, TGarbage) -> garbageRHS vb va
        _ -> return ()

-- * Rule right-hand sides

{- | β-reduction: delete the 'YaleApp' (@app@) and the 'YaleLam' (@lam@), wire
the body to the result, the argument to the variable, and introduce an
'Erase' (@d@) agent at the head of the free-variable list.

Uses the 'Rewrite' monad (not 'Replace') so that the fan-out logic can call
'deleteEdge' to split the binder edge into per-occurrence edges when the
bound variable is used non-linearly (Mackie 1998, duplication).
-}
betaRHS :: YaleNode -> YaleNode -> Node -> Node -> Rewrite YaleNode ()
betaRHS app lam appNode lamNode = do
  eD <- newEdge
  _ <- newNode Erase{eraseIn = eD}
  -- Fan-out (Mackie 1998, duplication): if the binder edge carries more than
  -- one bound-variable occurrence, the argument is used non-linearly and must
  -- be DUPLICATED. Split the binder edge into per-occurrence edges and create
  -- a chain of 'Copy' agents to duplicate the argument into one copy per
  -- occurrence. If there is exactly one occurrence, just splice the argument
  -- onto the binder edge (linear case).
  --
  -- This MUST run BEFORE the body\/result merge below: in the degenerate
  -- @body = var@ case (@lamBody == lamVar@), merging @appResult@ onto
  -- @lamBody@ first would bring the result edge's ports (e.g. the 'Initiator'
  -- output) onto the binder edge, and the occurrence count would mistake them
  -- for extra occurrences, spawning a spurious 'Copy' chain. Counting first
  -- sees only the genuine occurrences.
  let v = lamVar lam
  ns <- attachedNodes v
  fullPorts <- concat <$> mapM (portsOn v) ns
  -- Determine which ports are actual occurrences of the bound variable in the
  -- current λ's body:
  -- \* The current λ's lamVar port (index 2) is the binder, not an occurrence.
  -- \* The current λ's lamBody port (index 3) IS an occurrence if on v
  --   (degenerate case: body = var, e.g. λy. y).
  -- \* Other YaleLam nodes with the same binder edge are defensive exclusion
  --   (should not occur with the split-binder-edge design).
  -- \* All other ports on v are genuine body occurrences.
  isOcc <- mapM (isOccPort v lamNode) fullPorts
  let occPorts = [(n, i) | ((n, i), True) <- zip fullPorts isOcc]
  case occPorts of
    [] ->
      -- The bound variable is UNUSED: the argument is discarded. Attach a
      -- 'Garbage' (@e@) agent directly onto the argument edge to erase it
      -- (Mackie 1998: an unused argument is erased, not spliced onto the
      -- binder edge). 'newNode' is used (rather than 'newEdge'+'mergeEdges')
      -- so the argument edge is NOT deleted: in the tie-the-knot the argument
      -- edge is shared with the sibling app's ports, and deleting it would
      -- leave those ports' owner holding a stale edge reference.
      void $ newNode (Garbage{garbageIn = appArg app})
    -- Linear: one occurrence. Which edge survives the merge depends on the
    -- case, because 'mergeEdges' deletes its second argument and the loser's
    -- owner may hold a stale reference:
    --
    -- \* Degenerate @body = var@ (@lamBody == v@): the BINDER @v@ must survive
    --   (first argument). The body\/result merge below reads @lamBody lam@
    --   (= @v@); deleting @v@ would make that merge a silent no-op and strand
    --   the result. Here @appArg@ is the plain argument (not a shared knot
    --   edge), so deleting it is harmless.
    -- \* Non-degenerate (@lamBody /= v@): the ARGUMENT edge must survive (first
    --   argument). In the tie-the-knot the argument edge is shared with the
    --   sibling app's ports; deleting it would leave those ports' owner holding
    --   a stale edge reference. The body\/result merge below reads @lamBody lam@
    --   (/= @v@), a distinct edge that is not deleted here, so it still works.
    [_single] ->
      if lamBody lam == v
        then mergeEdges v (appArg app)
        else mergeEdges (appArg app) v
    _multi -> do
      es <- deleteEdge v
      let zipped = zip es fullPorts
      let eOccs = [e | (e, np) <- zipped, np `elem` occPorts]
      createCopyChain (appArg app) eOccs
  mergeEdges (lamBody lam) (appResult app)
  mergeEdges eD (lamVarList lam)
  deleteNode appNode
  deleteNode lamNode

{- | The ports of node @n@ that lie on edge @e@, as @(node, portIndex)@ pairs
in port-index order. Used to correlate the fresh edges returned by
'deleteEdge' (which are produced in @attachedNodes@-then-port-index order)
with the specific ports that were on the deleted edge.
-}
portsOn :: Edge -> Node -> Rewrite YaleNode [(Node, Int)]
portsOn e n = do
  ports <- inspectNode n
  return [(n, i) | (i, p) <- zip [0 ..] ports, p == e]

{- | Whether port @(n, i)@ on binder edge @v@ is an actual occurrence of the
bound variable in the body of the λ at @lamNode@:
* For the current λ: port 2 (lamVar) is the binder, not an occurrence;
  port 3 (lamBody) IS an occurrence if on @v@ (degenerate case: body = var).
* For other 'YaleLam' nodes with the same binder edge: excluded (defensive).
* For a 'FreeVar'\/'FreeVarBlocked' node: the @fvOcc@ port (which sits on the
  binder edge as the free-variable connection point) is NOT an occurrence —
  it is a passive receiver of the β-splice, not a use of the bound variable.
  Counting it would spuriously duplicate the argument (corrupting the
  tie-the-knot cycle). The list-continuation ports (@fvCont@\/@fvHead@) are
  not on the binder edge, so they never appear here.
* For all other nodes: included (genuine body occurrences).
-}
isOccPort :: Edge -> Node -> (Node, Int) -> Rewrite YaleNode Bool
isOccPort v lamNode (n, i)
  | n == lamNode = return (i /= 2)
  | otherwise = do
      nv <- readNode n
      case nv of
        -- FreeVar ports: [fvCont, fvOcc]; fvOcc (index 1) is not an occurrence.
        FreeVar{} -> return (i /= 1)
        -- FreeVarBlocked ports: [fvHead, fvCont, fvOcc]; fvOcc (index 2) is not.
        FreeVarBlocked{} -> return (i /= 2)
        _ -> return (not (isOtherCopyOf v nv))

{- | Whether @nv@ is a 'YaleLam' with the same binder edge @v@ (i.e. another
copy of the same abstraction).
-}
isOtherCopyOf :: Edge -> YaleNode -> Bool
isOtherCopyOf v nv = case nv of
  YaleLam{lamVar = v'} -> v' == v
  _ -> False

{- | Create a chain of 'Copy' agents to duplicate the term on edge @eIn@ into
one copy per edge in @eOccs@. For @k@ occurrence edges, creates @k-1@ 'Copy'
agents and @k-2@ temporary edges. The first Copy duplicates the term into the
first occurrence edge and a temporary edge; each middle Copy duplicates the
previous temporary edge into the next occurrence edge and a new temporary
edge; the final Copy duplicates the last temporary edge into the last two
occurrence edges.
-}
createCopyChain :: Edge -> [Edge] -> Rewrite YaleNode ()
createCopyChain eIn eOccs = case eOccs of
  [] -> return ()
  [eSingle] -> mergeEdges eIn eSingle
  [e1, e2] ->
    void $ newNode $ Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = [], copySplitVars = []}
  (e1 : eRest) -> do
    eTemp <- newEdge
    _ <- newNode Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = eTemp, copySticky = [], copySplitVars = []}
    createCopyChain eTemp eRest

{- | 'Copy'×'YaleLam': copy the abstraction (Mackie 1998, §5 "Duplication").
Two copies of the @λ@ are produced. A 'Copy' (@c@) agent is introduced to
duplicate the body ("a c agent is introduced to duplicate the body of the
abstraction, in the same way as we copied the abstraction agent"). A 'Subst'
(@s@) agent is propagated into each copy's body ("we propagate s agents inside
the body of the abstraction"). A 'Copy' (@c@) agent is propagated along the
free-variable list ("we also propagate the c agent along the list of free
variables"); for a closed term the list is just @ν@, so the @c@ agent simply
duplicates the end-of-list agent ("if the term is closed, then the following
reduction duplicates the end of list agent"). The bound-variable occurrence is
shared by both copies.
-}
copyLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyLamRHS c lam = do
  let v = lamVar lam
  let sticky = copySticky c
  let splitVars = copySplitVars c
  -- Each copy gets its OWN binder edge (split, not shared). This ensures that
  -- β-reduction on one copy does not interfere with the other: the binder edge
  -- of each copy only carries that copy's ports.
  v1 <- byEdge
  v2 <- byEdge
  let newSplitVars = (v, v1, v2) : splitVars
  eList1 <- byEdge
  eList2 <- byEdge
  -- If the body is just the bound variable (lamBody == v, e.g. @λy. y@), the
  -- copies' body ports sit directly on their respective binder edges (no body
  -- Copy agent needed — there is no sub-net to duplicate). Otherwise a body
  -- Copy agent duplicates the body sub-net into fresh edges, carrying the
  -- split-vars so that bound-variable occurrences are routed to the correct
  -- copy's binder edge.
  if lamBody lam == v
    then do
      byNode $ YaleLam{lamResult = copyOut1 c, lamVarList = eList1, lamVar = v1, lamBody = v1}
      byNode $ YaleLam{lamResult = copyOut2 c, lamVarList = eList2, lamVar = v2, lamBody = v2}
    else do
      eBody1 <- byEdge
      eBody2 <- byEdge
      byNode $ YaleLam{lamResult = copyOut1 c, lamVarList = eList1, lamVar = v1, lamBody = eBody1}
      byNode $ YaleLam{lamResult = copyOut2 c, lamVarList = eList2, lamVar = v2, lamBody = eBody2}
      byNode $
        Copy{copyIn = lamBody lam, copyOut1 = eBody1, copyOut2 = eBody2, copySticky = sticky, copySplitVars = newSplitVars}
  -- A c agent is propagated along the free-variable list. For a closed term
  -- the list is just ν, so the c agent duplicates the end-of-list agent.
  byNode $
    Copy{copyIn = lamVarList lam, copyOut1 = eList1, copyOut2 = eList2, copySticky = sticky, copySplitVars = splitVars}

{- | Port allocation when duplicating a node. Each auxiliary port is either:
(a) a bound-variable occurrence of an abstraction being duplicated — it sits on
one of the @original@ edges in @splitVars@ — in which case it is SPLIT: copy 1
gets @v1@ and copy 2 gets @v2@ (no 'Copy' agent is spawned);
(b) a bound-variable occurrence of an OUTER abstraction — it sits on one of the
enclosing binder edges in @sticky@ — in which case it is SHARED between the two
copies (the same edge is reused, no 'Copy' agent is spawned);
(c) an ordinary port — a fresh edge is allocated for each copy and a 'Copy'
agent is spawned to duplicate the port's content.

Returns @(edge1, edge2, needCopyAgent)@.
-}
byEdgeOrSticky :: [Edge] -> [(Edge, Edge, Edge)] -> Edge -> Replace YaleNode (Edge, Edge, Bool)
byEdgeOrSticky sticky splitVars port
  | Just (v1, v2) <- lookupSplit splitVars port = return (v1, v2, False)
  | port `elem` sticky = return (port, port, False)
  | otherwise = do
      e1 <- byEdge
      e2 <- byEdge
      return (e1, e2, True)
 where
  lookupSplit [] _ = Nothing
  lookupSplit ((orig, v1, v2) : rest) p
    | p == orig = Just (v1, v2)
    | otherwise = lookupSplit rest p

-- | Whether a port needs a 'Copy' agent (it is neither split nor sticky).
needsCopyAgent :: [Edge] -> [(Edge, Edge, Edge)] -> Edge -> Bool
needsCopyAgent sticky splitVars port
  | port `elem` sticky = False
  | otherwise = isNothing (lookupSplit splitVars port)
 where
  lookupSplit [] _ = Nothing
  lookupSplit ((orig, _, _) : rest) p
    | p == orig = Just ()
    | otherwise = lookupSplit rest p

{- | Create a chain of 'Copy' agents (in the 'Replace' monad) to duplicate the
term on edge @eIn@ into one copy per edge in @eTargets@. For @k@ target edges
this creates @k-1@ 'Copy' agents and @k-2@ temporary edges. Used when several
ports of a duplicated node share a single original edge (e.g. the
self-application @(x x)@, where @appFunc == appArg@): a single 'Copy' per port
would be consumed by the first and leave the rest stuck, so the term is fanned
out through a chain instead.
-}
createCopyChainReplace :: [Edge] -> [(Edge, Edge, Edge)] -> Edge -> [Edge] -> Replace YaleNode ()
createCopyChainReplace sticky splitVars eIn eTargets = case eTargets of
  [] -> return ()
  [e1, e2] ->
    byNode $ Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = sticky, copySplitVars = splitVars}
  (e1 : eRest) -> do
    eTemp <- byEdge
    byNode $ Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = eTemp, copySticky = sticky, copySplitVars = splitVars}
    createCopyChainReplace sticky splitVars eTemp eRest

{- | 'Copy'×'YaleApp': copy the application. Two copies of the @@@ are
produced; a 'Copy' (@c@) agent is propagated along each copy's auxiliary
ports (the function and the argument) — unless the port is a bound-variable
occurrence (on a sticky edge), in which case it is shared.

When the function and argument ports share the SAME original edge (the
self-application @(x x)@), a single 'Copy' per port would be consumed by the
first and leave the second stuck (empty input). Instead the term is fanned out
to all four target edges through a 'Copy' chain ('createCopyChainReplace').
-}
copyAppRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyAppRHS c app = do
  let sticky = copySticky c
  let splitVars = copySplitVars c
  (eFunc1, eFunc2, needFuncCopy) <- byEdgeOrSticky sticky splitVars (appFunc app)
  (eArg1, eArg2, needArgCopy) <- byEdgeOrSticky sticky splitVars (appArg app)
  byNode $ YaleApp{appFunc = eFunc1, appArg = eArg1, appResult = copyOut1 c}
  byNode $ YaleApp{appFunc = eFunc2, appArg = eArg2, appResult = copyOut2 c}
  -- c is propagated along the function and argument ports: each new c agent
  -- sits on the original port edge and fans it out to the two copies. A port
  -- that is a bound-variable occurrence (split or sticky) is not duplicated.
  -- If both ports share the same original edge (self-application), fan the
  -- term out to all four targets with a Copy chain (see above).
  case (needFuncCopy, needArgCopy) of
    (True, True)
      | appFunc app == appArg app ->
          createCopyChainReplace sticky splitVars (appFunc app) [eFunc1, eFunc2, eArg1, eArg2]
    (True, _) -> do
      byNode $
        Copy{copyIn = appFunc app, copyOut1 = eFunc1, copyOut2 = eFunc2, copySticky = sticky, copySplitVars = splitVars}
      when needArgCopy $
        byNode $
          Copy{copyIn = appArg app, copyOut1 = eArg1, copyOut2 = eArg2, copySticky = sticky, copySplitVars = splitVars}
    (False, True) ->
      byNode $ Copy{copyIn = appArg app, copyOut1 = eArg1, copyOut2 = eArg2, copySticky = sticky, copySplitVars = splitVars}
    (False, False) -> return ()

-- | 'Copy'×'Constant': copy a base value. Two copies.
copyConstRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyConstRHS c k = do
  byNode $ Constant{inp = copyOut1 c, name = name k}
  byNode $ Constant{inp = copyOut2 c, name = name k}

-- | 'Copy'×'EndList': copy the end-of-list. Two @ν@'s.
copyEndRHS :: YaleNode -> Replace YaleNode ()
copyEndRHS c = do
  byNode $ EndList{endIn = copyOut1 c}
  byNode $ EndList{endIn = copyOut2 c}

{- | 'Copy'×'FreeVar': duplicate the @b@ agent (a free-variable list element).
Two @b@ copies are produced (sharing the variable occurrence), and a @c@
agent is propagated along the list continuation (the tail).
-}
copyFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarRHS c b = do
  byNode $ FreeVar{fvCont = copyOut1 c, fvOcc = fvOcc b, name = name b}
  byNode $ FreeVar{fvCont = copyOut2 c, fvOcc = fvOcc b, name = name b}
  byNode $
    Copy
      { copyIn = fvCont b
      , copyOut1 = copyOut1 c
      , copyOut2 = copyOut2 c
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }

{- | 'Copy'×'FreeVarBlocked': the blocking rule. A @c@ agent meeting a @b'@
agent converts the @b'@ back to a @b@ (Mackie 1998: "in the process of
duplication we also convert the b' agent back to a b agent"), duplicates
it (two @b@ copies sharing the variable occurrence), and propagates a @c@
agent along the list continuation (the tail).
-}
copyFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarBlockedRHS c b = do
  byNode $ FreeVar{fvCont = copyOut1 c, fvOcc = fvOcc b, name = name b}
  byNode $ FreeVar{fvCont = copyOut2 c, fvOcc = fvOcc b, name = name b}
  byNode $
    Copy
      { copyIn = fvCont b
      , copyOut1 = copyOut1 c
      , copyOut2 = copyOut2 c
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }

{- | 'Subst'×'YaleApp': the @s@ agent propagates into the application's
function and argument. The application is preserved (recreated) with its
result rewired to the @s@ output.
-}
substAppRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substAppRHS s app = do
  eS1 <- byEdge
  eS2 <- byEdge
  byNode $ YaleApp{appFunc = eS1, appArg = eS2, appResult = substOut s}
  byNode $ Subst{substIn = appFunc app, substOut = eS1, substVar = substVar s, substVarCopy = substVarCopy s}
  byNode $ Subst{substIn = appArg app, substOut = eS2, substVar = substVar s, substVarCopy = substVarCopy s}

{- | 'Subst'×'YaleLam': the @s@ agent propagates into the abstraction's body.
The result, bound variable, and free-variable list are preserved. The result
is kept on the meeting edge (@substIn@) to avoid a self-loop when the @s@
output coincides with the @λ@'s body port (as in 'copyLamRHS').
-}
substLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substLamRHS s lam = do
  eS <- byEdge
  byNode $ YaleLam{lamResult = substOut s, lamVarList = lamVarList lam, lamVar = lamVar lam, lamBody = eS}
  byNode $ Subst{substIn = lamBody lam, substOut = eS, substVar = substVar s, substVarCopy = substVarCopy s}

{- | 'Subst'×'Constant': the @s@ agent is erased at a base value; the base
value is preserved (recreated) with its input rewired to the @s@ output.
-}
substConstRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substConstRHS s k = do
  byNode $ Constant{inp = substOut s, name = name k}

{- | 'Subst'×'BoundVar': the @s@ agent resolves a bound-variable occurrence via
the implicit @x[v\/x] ↝ v@ rule (Mackie 1998: "the translation of the rule
x[v\/x] ↝w v is implicit … no interaction rules are required"). The @s@ agent
is erased; a new 'BoundVar' marker is emitted whose @bvOcc@ port continues the
@s@ output (the occurrence stays in the body) and whose @bvEdge@ port is rewired
to the copy's own bound-variable edge when the marker belongs to the abstraction
being duplicated (matched by the stable @bvId@), or kept on the original binder
edge otherwise (a nested abstraction's bound variable passes through).
-}
substBoundVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substBoundVarRHS s bv =
  if bvId bv == substVar s
    then do
      -- The bound variable is the current abstraction's (matched by the stable
      -- @bvId@): rewire the occurrence to the copy's own bound-variable edge
      -- (so the copy does not share the original's edge, which would be
      -- corrupted by a later β-splice).
      byNode $ BoundVar{bvOcc = substOut s, bvEdge = substVarCopy s, bvId = bvId bv}
    else do
      -- A nested abstraction's bound variable: pass through, keeping the
      -- original binder edge.
      byNode $ BoundVar{bvOcc = substOut s, bvEdge = bvEdge bv, bvId = bvId bv}

-- | 'Erase'×(EndList\/FreeVarBlocked): the @d@ agent erases the list element.
eraseRHS :: Replace YaleNode ()
eraseRHS = return ()

{- | 'Garbage'×(any term): the @e@ agent erases the term and RECURSIVELY the
net on its auxiliary ports (Mackie 1998, garbage collection). The two nodes
that met (@e@ and the term) are deleted by the 'replace' action; the RHS
spawns a fresh 'Garbage' agent on each auxiliary port so the erasure
propagates through the whole sub-net.

The binder edge of a 'YaleLam' (@lamVar@) is deliberately NOT given a fresh
'Garbage': it is a hyperedge shared with the body's bound-variable
occurrences. Those occurrence ports are removed as the body's nodes are
erased (via the 'Garbage' propagated onto @lamBody@), so the binder edge
drains naturally without a dedicated eraser (which would risk meeting
non-garbage ports spliced onto the shared edge).
-}
garbageRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
garbageRHS _ x = case x of
  YaleLam{} -> spawnGarbageOn [lamVarList x, lamBody x]
  YaleApp{} -> spawnGarbageOn [appArg x, appResult x]
  Copy{} -> spawnGarbageOn [copyOut1 x, copyOut2 x]
  Subst{} -> spawnGarbageOn [substOut x]
  FreeVar{} -> spawnGarbageOn [fvOcc x]
  FreeVarBlocked{} -> spawnGarbageOn [fvCont x, fvOcc x]
  BoundVar{} -> spawnGarbageOn [bvEdge x]
  -- Single-port agents with no auxiliary sub-net: just cancel.
  Erase{} -> return ()
  EndList{} -> return ()
  Constant{} -> return ()
  Multiplexer{} -> return ()
  Initiator{} -> error "Garbage met the Initiator (root is not collectable)"

{- | Attach a fresh 'Garbage' agent directly onto each of the given edges
(via 'byNode', so no new edge is created).
-}
spawnGarbageOn :: [Edge] -> Replace YaleNode ()
spawnGarbageOn = mapM_ (byNode . \e -> Garbage{garbageIn = e})

-- * Helpers

-- | A tag for the kind of node, used to dispatch the interaction.
data Tag
  = TApp
  | TLam
  | TCopy
  | TSubst
  | TErase
  | TEnd
  | TFreeV
  | TFVB
  | TConst
  | TBV
  | TGarbage
  deriving (Show)

-- | The tag of a node.
tag = \case
  YaleApp{} -> TApp
  YaleLam{} -> TLam
  Copy{} -> TCopy
  Subst{} -> TSubst
  Erase{} -> TErase
  EndList{} -> TEnd
  FreeVar{} -> TFreeV
  FreeVarBlocked{} -> TFVB
  Constant{} -> TConst
  BoundVar{} -> TBV
  Garbage{} -> TGarbage
  _ -> error "tag: unexpected node"

-- | Whether a node participates in the YALE interactions.
interacting :: YaleNode -> Bool
interacting = \case
  YaleLam{} -> True
  YaleApp{} -> True
  Copy{} -> True
  Subst{} -> True
  Erase{} -> True
  EndList{} -> True
  FreeVar{} -> True
  FreeVarBlocked{} -> True
  BoundVar{} -> True
  Garbage{} -> True
  Constant{} -> True
  _ -> False

-- | Whether two interacting nodes form a valid interaction pair.
interacts :: YaleNode -> YaleNode -> Bool
interacts va vb =
  isBeta va vb
    || isCopyPair va vb
    || isSubstPair va vb
    || isErasePair va vb
    || isGarbagePair va vb
 where
  isBeta x y = (isApp x && isLam y) || (isLam x && isApp y)
  -- A 'Copy' meets a term. The bound-variable marker is NOT a copy partner:
  -- its principal port is on the body edge (an occurrence), so a binder-edge
  -- 'Copy' never meets it (see the 'BoundVar' doc in YaleNet).
  isCopyPair x y = (isCopy x && isTerm y) || (isTerm x && isCopy y)
  isSubstPair x y = (isSubst x && isSubstTarget y) || (isSubstTarget x && isSubst y)
  isErasePair x y =
    (isErase x && (isEnd y || isFreeV y || isFVB y))
      || ((isEnd x || isFreeV x || isFVB x) && isErase y)
  isGarbagePair x y = (isGarbage x && isTerm y) || (isTerm x && isGarbage y)
  -- NB: 'BoundVar' is deliberately NOT a term: it is a passive marker on the
  -- bound-variable edge that the @c@ (copy) agent passes through (so the copy
  -- duplicates the actual argument term, not the marker) but that the @s@
  -- (substitution) agent resolves (see 'isSubstTarget').
  isTerm = \case
    YaleLam{} -> True
    YaleApp{} -> True
    Constant{} -> True
    EndList{} -> True
    FreeVar{} -> True
    FreeVarBlocked{} -> True
    _ -> False
  isApp (YaleApp{}) = True
  isApp _ = False
  isLam (YaleLam{}) = True
  isLam _ = False
  isCopy (Copy{}) = True
  isCopy _ = False
  isSubst (Subst{}) = True
  isSubst _ = False
  -- The s agent only propagates into term nodes (not list elements).
  isSubstTarget = \case
    YaleLam{} -> True
    YaleApp{} -> True
    Constant{} -> True
    BoundVar{} -> True
    _ -> False
  isErase (Erase{}) = True
  isErase _ = False
  isEnd (EndList{}) = True
  isEnd _ = False
  isFreeV (FreeVar{}) = True
  isFreeV _ = False
  isFVB (FreeVarBlocked{}) = True
  isFVB _ = False
  isGarbage (Garbage{}) = True
  isGarbage _ = False
