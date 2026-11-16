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
  abstraction: two copies of the @λ@ are produced, a 'Copy' is propagated
  along each copy's free-variable list, and two 'Delta' (@δ@) agents are
  introduced to duplicate the binder edge and the body.
* 'Copy'×'YaleApp' (Mackie §5, "Duplication") — the @c@ agent copies the
  application: two copies of the @@@ are produced, and a 'Copy' is propagated
  along each copy's auxiliary ports.
* 'Copy'×'Constant' (Mackie §5, "Duplication") — the @c@ agent copies a base
  value: two copies.
* 'Copy'×'EndList' (Mackie §5, "Duplication") — the @c@ agent copies the
  end-of-list: two @ν@'s.
* 'Delta'×'Delta' (Mackie §5, "Duplication") — two @δ@ agents meet and cancel
  (the duplication is complete); the four auxiliary ports are crossed.
* 'Delta'×@α@ (Mackie §5, "Duplication", final rule scheme) — the @δ@ agent
  duplicates the agent @α@ and propagates a fresh @δ@ along every auxiliary
  port of @α@.
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
module Lamagraph.Ski.YaleReduce (reduceStepsYale, reduceStepsYaleTrace, findActivePairs, fireInteraction) where

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
  byEdge,
  byNode,
  byWire,
  deleteNode,
  evalGraph,
  inspect,
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
  update,
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

{- | Like 'reduceStepsYale', but returns the SEQUENCE of intermediate nets:
the net after the compile-time binders are compiled away (before any
interaction), then the net after each interaction, up to the normal form (or
the step budget). Useful for visual debugging (see
'Lamagraph.Ski.YaleDraw.dumpReduction'): the @i@-th element is the net after
@i@ interactions.
-}
reduceStepsYaleTrace :: Int -> Graph YaleNode -> [Graph YaleNode]
reduceStepsYaleTrace maxSteps g0 =
  let g1 = compileBinders g0
   in go g1 maxSteps
 where
  go :: Graph YaleNode -> Int -> [Graph YaleNode]
  go g remaining
    | remaining <= 0 = [g]
    | otherwise = case findActivePairs g of
        [] -> [g]
        (a, b) : _ -> g : go (fireInteraction g a b) (remaining - 1)

{- | All active pairs of the net: unordered pairs of distinct interacting nodes
joined at their principal (port 0) edge, where the pair actually interacts.
The order is the deterministic node order — NO agent-type priority. Interaction
nets are confluent: the final normal form is independent of the reduction
order, so steering the order (e.g. firing 'Copy' pairs before β) is both
unnecessary and a symptom of non-confluent rules.
-}
findActivePairs :: Graph YaleNode -> [(Node, Node)]
findActivePairs = runReader go
 where
  go :: Reader (Graph YaleNode) [(Node, Node)]
  go = do
    g <- ask
    ns <- readNodeList
    vs <- mapM readNode ns
    let tagged = [(a, va) | (a, va) <- zip ns vs, interacting va]
    return
      [ (a, b)
      | (a, va) <- tagged
      , (b, vb) <- tagged
      , b > a
      , canMeet va vb
      , interacts va vb
      , -- Paper-faithful β precondition (Mackie 1998, Fig. 2): β-reduction
      -- fires only when the abstraction's free variables are all
      -- substituted. A 'FreeVar' whose @fvOcc@ sits on a SHARED edge
      -- (the tie-the-knot loop) is already wired to its value (the knot),
      -- so it counts as substituted. A 'FreeVar' on a plain edge is not
      -- yet substituted; firing β early would let the @d@ agent erase it
      -- and the @Garbage@ walk into shared structure (over-erasure).
      betaAllowed g va vb
      , betaAllowed g vb va
      ]
  -- β is allowed when the λ's free variables are all substituted: either
  -- the list is empty (no 'FreeVar' agents) or every 'FreeVar' points to a
  -- SHARED edge (the tie-the-knot loop), meaning it is already wired to its
  -- value. See the precondition comment in the pairs comprehension above.
  betaAllowed gg (YaleApp{}) (YaleLam{lamVarList = eList}) =
    allFvSubstituted gg eList
  betaAllowed gg (YaleLam{lamVarList = eList}) (YaleApp{}) =
    allFvSubstituted gg eList
  betaAllowed _ _ _ = True

{- | Whether every 'FreeVar' on the free-variable list edge @eList@ is
substituted, i.e. its @fvOcc@ port sits on a SHARED edge (more than two
ports — the tie-the-knot loop, where the value is already wired in). An empty
list (no 'FreeVar' agents) is trivially all-substituted. This is the net-level
form of the paper's β precondition @fv(t) = {x}@ (Mackie 1998, Fig. 2): the
body has no unsubstituted free variables, so the @d@ agent introduced by β
meets the 'EndList' (ν) or a knot-wired 'FreeVar' rather than draining an
unsubstituted free variable out of a shared net.
-}
allFvSubstituted :: Graph YaleNode -> Edge -> Bool
allFvSubstituted g eList =
  let m = do
        ns <- attachedNodes eList
        vs <- mapM readNode ns
        let fvEdges = [e | FreeVar{fvOcc = e} <- vs]
        shared <- mapM countEdgePorts fvEdges
        return (all (> 2) shared)
   in evalGraph m g

{- | Whether two agents can meet: they share an edge and at least one of them
 is on its principal port (standard interaction-net semantics, Mackie 1998).
 The common case is that both are on their principal ports. There is one
 deliberate exception: a 'Copy' (@c@) or 'Delta' (@δ@) agent may meet a
 'YaleApp' (@@@) at the application's result port, which is an auxiliary port
 of the @@@. This is how the body-copy and body-duplication agents introduced
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
  -- Guard: a Copy must NOT meet the application when the application's
  -- argument is the Copy's own output. That is the tie-the-knot cycle (Mackie
  -- §6): the Copy's output feeds the application's argument and the
  -- application's result feeds the Copy's input. Firing 'Copy'×'YaleApp' there
  -- would duplicate the application and propagate a 'Copy' along its function
  -- (the recursive body), re-entering the cycle and duplicating the body
  -- without bound (the knot never reaches normal form). The knot must instead
  -- be unzipped by the β-reduction of the application's function, which this
  -- guard leaves free to fire. In the legitimate body-copy case the
  -- application's argument is an ordinary term, never the Copy's own output,
  -- so the guard is inert there.
  agentAtAppResult a@(Copy{copyOut1 = o1, copyOut2 = o2}) (YaleApp{appResult = r, appArg = arg}) =
    principalPort a == r && arg /= o1 && arg /= o2
  -- The δ agent sits on the body edge (introduced by 'copyLamRHS') and must
  -- meet the body application at its result port to duplicate it.
  agentAtAppResult a@(Delta{}) (YaleApp{appResult = r}) = principalPort a == r
  -- The forcing agent sits on the (free) result edge and must meet the
  -- application there to walk into the function's net.
  agentAtAppResult a@(Forcing{}) (YaleApp{appResult = r}) = principalPort a == r
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
        -- Duplication: δ × X
        (TDelta, TDelta) -> deltaDeltaRHS va vb
        (TDelta, TApp) -> deltaAppRHS va vb
        (TApp, TDelta) -> deltaAppRHS vb va
        (TDelta, _) -> deltaAlphaRHS va vb
        (_, TDelta) -> deltaAlphaRHS vb va
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
        -- Forcing: 4 × X (the Garbage cases above already matched
        -- (Forcing, Garbage) and (Garbage, Forcing))
        (TForce, TForce) -> forcingCancelRHS
        (TForce, TFreeV) -> forcingFreeVarRHS va vb
        (TFreeV, TForce) -> forcingFreeVarRHS vb va
        (TForce, TFVB) -> forcingFreeVarBlockedRHS va vb
        (TFVB, TForce) -> forcingFreeVarBlockedRHS vb va
        (TForce, _) -> forcingPassRHS va vb
        (_, TForce) -> forcingPassRHS vb va
        _ -> return ()

-- * Rule right-hand sides

{- | β-reduction (Mackie 1998, LINEAR encoding): a pure local replacement.
Delete the 'YaleApp' (@app@) and the 'YaleLam' (@lam@), splice the argument
onto the binder edge (which carries exactly ONE port into the body — a direct
wire for a single occurrence, a 'Copy' (@c@) agent for ≥2, or a 'Garbage'
(@e@) agent for 0), wire the body to the result, and introduce an 'Erase'
(@d@) agent at the head of the free-variable list.

The binder edge has exactly one port (guaranteed by the compiler's linear
encoding), so there is NO fan-out here: the 'Copy'\/'Garbage' agents emitted
by the compiler handle duplication\/erasure. This rule is a pure splice with
no metadata and no additional knowledge.
-}
betaRHS :: YaleNode -> YaleNode -> Node -> Node -> Rewrite YaleNode ()
betaRHS app lam appNode lamNode = do
  eD <- newEdge
  _ <- newNode Erase{eraseIn = eD}
  if lamBody lam == lamVar lam
    then do
      -- Degenerate @body = var@ (e.g. @λx. x@): the body IS the binder edge.
      -- Wire the argument directly to the result (the single occurrence port
      -- on the binder edge is effectively the body).
      mergeEdges (appArg app) (appResult app)
    else do
      -- Normal case: splice the argument onto the binder edge (which carries
      -- exactly one port into the body — a direct wire, 'Copy', or 'Garbage'),
      -- then wire the body to the result.
      mergeEdges (appArg app) (lamVar lam)
      mergeEdges (lamBody lam) (appResult app)
  -- Introduce the Erase agent on the varlist.
  mergeEdges eD (lamVarList lam)
  -- Delete the λ and @ agents.
  deleteNode appNode
  deleteNode lamNode

{- | The ports of node @n@ that lie on edge @e@, as @(node, portIndex)@ pairs
in port-index order. Used to correlate the fresh edges produced by an edge
deletion (which are emitted in @attachedNodes@-then-port-index order) with the
specific ports that were on the deleted edge.
-}
portsOn :: Edge -> Node -> Rewrite YaleNode [(Node, Int)]
portsOn e n = do
  ports <- inspectNode n
  return [(n, i) | (i, p) <- zip [0 ..] ports, p == e]

{- | The total number of ports on edge @e@ (summed over all attached nodes).
A plain (binary) edge has exactly two ports; a SHARED edge (hyperedge — e.g.
the tie-the-knot loop, or a binder edge with multiple bound-variable
occurrences) has more than two. Used to detect shared edges that must not be
erased by a 'Garbage' agent (see the unused-variable case in 'betaRHS').
-}
countEdgePorts :: Edge -> Rewrite YaleNode Int
countEdgePorts e = do
  ns <- attachedNodes e
  ps <- concat <$> mapM (portsOn e) ns
  return (length ps)

{- | 'Copy'×'YaleLam': copy the abstraction (Mackie 1998, §5 "Duplication",
the @c@×@λ@ rule). Two copies of the @λ@ are produced, each with its own fresh
binder edge and body edge. A 'Copy' (@c@) agent is propagated along the
free-variable list ("we also propagate the c agent along the list of free
variables"); for a closed term the list is just @ν@, so the @c@ agent simply
duplicates the end-of-list agent. Two 'Delta' (@δ@) agents are introduced: one
duplicates the binder edge (the bound-variable occurrence) and one duplicates
the body ("a δ agent is introduced to duplicate the body of the abstraction,
in the same way as we copied the abstraction agent"). This rule is a pure
local replacement with no metadata.
-}
copyLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyLamRHS c lam = do
  let v = lamVar lam
  let bodyEdge = lamBody lam
  v1 <- byEdge
  v2 <- byEdge
  eList1 <- byEdge
  eList2 <- byEdge
  eBody1 <- byEdge
  eBody2 <- byEdge
  -- Two copies of the λ, each with its own fresh binder edge and body edge.
  byNode $ YaleLam{lamResult = copyOut1 c, lamVarList = eList1, lamVar = v1, lamBody = eBody1}
  byNode $ YaleLam{lamResult = copyOut2 c, lamVarList = eList2, lamVar = v2, lamBody = eBody2}
  -- δ agents: one duplicates the binder edge (the bound-variable occurrence),
  -- one duplicates the body.
  byNode $ Delta{deltaIn = v, deltaOut1 = v1, deltaOut2 = v2}
  byNode $ Delta{deltaIn = bodyEdge, deltaOut1 = eBody1, deltaOut2 = eBody2}
  -- Propagate the c agent along the free-variable list.
  byNode $
    Copy{copyIn = lamVarList lam, copyOut1 = eList1, copyOut2 = eList2, copySticky = [], copySplitVars = []}

{- | Look up an @original@ binder edge in the split-vars, returning the
@(v1, v2)@ split edges if found. Shared by the port-allocation helper
('byEdgeOrSticky') and the FreeVar occurrence routing ('routeFvOcc').
-}
lookupSplitEdge :: [(Edge, Edge, Edge)] -> Edge -> Maybe (Edge, Edge)
lookupSplitEdge [] _ = Nothing
lookupSplitEdge ((orig, v1, v2) : rest) p
  | p == orig = Just (v1, v2)
  | otherwise = lookupSplitEdge rest p

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
  | Just (v1, v2) <- lookupSplitEdge splitVars port = return (v1, v2, False)
  | port `elem` sticky = return (port, port, False)
  | otherwise = do
      e1 <- byEdge
      e2 <- byEdge
      return (e1, e2, True)

{- | Route the @fvOcc@ edge of a 'FreeVar' through the split-vars when the
abstraction that binds the variable is itself being duplicated. The @fvOcc@ is
a passive receiver of the enclosing β-splice (the value flows in when the
enclosing abstraction is applied), so — exactly like a bound-variable
occurrence handled by 'byEdgeOrSticky' — it must be SPLIT when the enclosing
binder edge is in the split-vars: copy 1 gets @v1@ and copy 2 gets @v2@, so
each copy's 'FreeVar' points to its own copy's binder edge. If the enclosing
abstraction is NOT being duplicated (the @fvOcc@ is not an @original@ edge in
the split-vars), the @fvOcc@ is SHARED (both copies point to the same binder
edge). Sharing it in the duplicated case would leave both copies' 'FreeVar's
pointing at the original binder edge, which is drained when one copy is
β-applied (the argument splices onto the copy's split edge, not the original),
so the value never reaches the other copy and its redex deadlocks.
-}
routeFvOcc :: [(Edge, Edge, Edge)] -> Edge -> (Edge, Edge)
routeFvOcc splitVars fvOcc =
  case lookupSplitEdge splitVars fvOcc of
    Just (v1, v2) -> (v1, v2)
    Nothing -> (fvOcc, fvOcc)

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
Two @b@ copies are produced, and a @c@ agent is propagated along the list
continuation (the tail). The variable occurrence (@fvOcc@) is routed through
the split-vars by 'routeFvOcc': if the abstraction that binds the variable is
being duplicated, each copy's @fvOcc@ points to its own copy's binder edge;
otherwise the @fvOcc@ is shared.
-}
copyFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarRHS c b = do
  let (fvOcc1, fvOcc2) = routeFvOcc (copySplitVars c) (fvOcc b)
  byNode $ FreeVar{fvCont = copyOut1 c, fvOcc = fvOcc1, name = name b}
  byNode $ FreeVar{fvCont = copyOut2 c, fvOcc = fvOcc2, name = name b}
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
duplication we also convert the b' agent back to a b agent"), duplicates it
(two @b@ copies, each with its @fvOcc@ routed by 'routeFvOcc'), and propagates
a @c@ agent along the list continuation (the tail).
-}
copyFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarBlockedRHS c b = do
  let (fvOcc1, fvOcc2) = routeFvOcc (copySplitVars c) (fvOcc b)
  byNode $ FreeVar{fvCont = copyOut1 c, fvOcc = fvOcc1, name = name b}
  byNode $ FreeVar{fvCont = copyOut2 c, fvOcc = fvOcc2, name = name b}
  byNode $
    Copy
      { copyIn = fvCont b
      , copyOut1 = copyOut1 c
      , copyOut2 = copyOut2 c
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }

{- | 'Delta'×'Delta': two @δ@ agents meet and cancel (Mackie 1998, §5
"Duplication"). This indicates that the duplication process is complete. The
four auxiliary ports are crossed.
-}
deltaDeltaRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaDeltaRHS d1 d2 =
  byWire (deltaOut1 d1) (deltaOut2 d2)
    <> byWire (deltaOut2 d1) (deltaOut1 d2)

{- | 'Delta'×'YaleApp': the @δ@ agent duplicates an application. Because the
application's principal is @appFunc@ (so that β-reduction is a principal-port
interaction), the @δ@ — which is spawned on the body\/result edge — meets the
application at its @appResult@ port (via the @appResult@ meeting exception),
NOT at the principal. The meeting port must be the one wired to the @δ@'s two
outputs; the other two ports each receive a propagated @δ@. Wiring the wrong
port (as the generic @δ@×@α@ rule would) disconnects the function and feeds the
duplication back into itself, growing the net without bound.
-}
deltaAppRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaAppRHS d app =
  if principalPort d == appResult app
    then -- δ on @appResult@ (the common case): the copies' results feed the
    -- @δ@'s outputs; the function and argument are each duplicated.
      do
        eF1 <- byEdge
        eF2 <- byEdge
        eA1 <- byEdge
        eA2 <- byEdge
        byNode $ YaleApp{appFunc = eF1, appArg = eA1, appResult = deltaOut1 d}
        byNode $ YaleApp{appFunc = eF2, appArg = eA2, appResult = deltaOut2 d}
        byNode $ Delta{deltaIn = appFunc app, deltaOut1 = eF1, deltaOut2 = eF2}
        byNode $ Delta{deltaIn = appArg app, deltaOut1 = eA1, deltaOut2 = eA2}
    else -- δ on @appFunc@ (the principal): the copies' functions feed the
    -- @δ@'s outputs; the argument and result are each duplicated.
      do
        eA1 <- byEdge
        eA2 <- byEdge
        eB1 <- byEdge
        eB2 <- byEdge
        byNode $ YaleApp{appFunc = deltaOut1 d, appArg = eA1, appResult = eB1}
        byNode $ YaleApp{appFunc = deltaOut2 d, appArg = eA2, appResult = eB2}
        byNode $ Delta{deltaIn = appArg app, deltaOut1 = eA1, deltaOut2 = eA2}
        byNode $ Delta{deltaIn = appResult app, deltaOut1 = eB1, deltaOut2 = eB2}

{- | 'Delta'×@α@: the general duplication rule (Mackie 1998, §5 "Duplication",
final rule scheme). The @δ@ agent duplicates the agent @α@ into two copies and
propagates a fresh @δ@ agent along every auxiliary port of @α@, so that the
duplication continues into the sub-net. The two copies' principal ports are
wired to the @δ@'s two outputs. This is a pure local replacement with no
metadata.
-}
deltaAlphaRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaAlphaRHS d alpha = do
  let ports :: [Edge] = inspect alpha -- [principal, aux_1, ..., aux_n]
  let aux = drop 1 ports -- [aux_1, ..., aux_n]
  e1s <- mapM (const byEdge) aux
  e2s <- mapM (const byEdge) aux
  byNode $ update (deltaOut1 d : e1s) alpha
  byNode $ update (deltaOut2 d : e2s) alpha
  for_ (zip3 aux e1s e2s) $ \(origE, e1, e2) ->
    byNode $ Delta{deltaIn = origE, deltaOut1 = e1, deltaOut2 = e2}

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
  Delta{} -> spawnGarbageOn [deltaOut1 x, deltaOut2 x]
  FreeVar{} -> spawnGarbageOn [fvOcc x]
  FreeVarBlocked{} -> spawnGarbageOn [fvCont x, fvOcc x]
  -- Single-port agents with no auxiliary sub-net: just cancel.
  Erase{} -> return ()
  EndList{} -> return ()
  Constant{} -> return ()
  Forcing{} -> return ()
  -- e × e: the interaction marks the end of the erasing process (Mackie §5);
  -- both agents cancel, leaving the empty net.
  Garbage{} -> return ()
  Multiplexer{} -> return ()
  Initiator{} -> error "Garbage met the Initiator (root is not collectable)"

{- | Attach a fresh 'Garbage' agent directly onto each of the given edges
(via 'byNode', so no new edge is created).
-}
spawnGarbageOn :: [Edge] -> Replace YaleNode ()
spawnGarbageOn = mapM_ (byNode . \e -> Garbage{garbageIn = e})

-- * Forcing agent (Mackie §5.1, "Strong Reduction", Fig. 4)

{- | The "body" edge of an agent: the edge the forcing agent moves to when it
passes through the agent. For a 'YaleLam' it is the body; for a 'YaleApp' it
is the result; for a 'Copy'\/'Delta'\/'Erase'\/'Garbage' it is the input.
Leaves (a 'Constant' or 'EndList' with no auxiliary sub-net) have no body
edge: the forcing agent is absorbed.
-}
forcingBody :: YaleNode -> Maybe Edge
-- The forcing agent walks into the FREE-VARIABLE LIST of the abstraction:
-- that is where the @b@ agents live, and they are the ones that must be
-- toggled to @b'@ to allow blocked reductions to complete.
forcingBody (YaleLam{lamVarList = l}) = Just l
-- The forcing agent walks into the FUNCTION's net (the computation continues
-- there); the argument is a value, not a continuation.
forcingBody (YaleApp{appFunc = f}) = Just f
forcingBody (Copy{copyIn = c}) = Just c
forcingBody (Delta{deltaIn = s}) = Just s
forcingBody (Erase{eraseIn = e}) = Just e
forcingBody (Garbage{garbageIn = g}) = Just g
forcingBody (Constant{}) = Nothing
forcingBody (EndList{}) = Nothing
forcingBody _ = Nothing

{- | 'Forcing'×'Forcing': the two forcing agents cancel (the forcing is
complete).
-}
forcingCancelRHS :: Replace YaleNode ()
forcingCancelRHS = return ()

{- | 'Forcing'×'FreeVar': convert the @b@ agent into a @b'@ (blocked
free-variable) agent, which allows other reductions to complete. The forcing
agent is absorbed; the @b'@ agent's new principal port (@fvHead@) is wired to
the interaction edge (the forcing agent's former port).
-}
forcingFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
forcingFreeVarRHS f b =
  byNode $ FreeVarBlocked{fvHead = forcingIn f, fvCont = fvCont b, fvOcc = fvOcc b, name = name b}

{- | 'Forcing'×'FreeVarBlocked': convert the @b'@ agent back to a @b@ agent.
The forcing agent is absorbed.
-}
forcingFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
forcingFreeVarBlockedRHS _ b =
  byNode $ FreeVar{fvCont = fvCont b, fvOcc = fvOcc b, name = name b}

{- | 'Forcing'×(other agent): pass-through. The partner is recreated
unchanged; the forcing agent moves to the partner's body edge. If the partner
is a leaf (no body edge), the forcing agent is absorbed.
-}
forcingPassRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
forcingPassRHS _ x = case forcingBody x of
  Just e -> do
    byNode x
    byNode $ Forcing{forcingIn = e}
  Nothing -> do
    byNode x
    return ()

-- * Helpers

-- | A tag for the kind of node, used to dispatch the interaction.
data Tag
  = TApp
  | TLam
  | TCopy
  | TDelta
  | TErase
  | TEnd
  | TFreeV
  | TFVB
  | TConst
  | TGarbage
  | TForce
  deriving (Show)

-- | The tag of a node.
tag :: YaleNode -> Tag
tag = \case
  YaleApp{} -> TApp
  YaleLam{} -> TLam
  Copy{} -> TCopy
  Delta{} -> TDelta
  Erase{} -> TErase
  EndList{} -> TEnd
  FreeVar{} -> TFreeV
  FreeVarBlocked{} -> TFVB
  Constant{} -> TConst
  Garbage{} -> TGarbage
  Forcing{} -> TForce
  _ -> error "tag: unexpected node"

-- | Whether a node participates in the YALE interactions.
interacting :: YaleNode -> Bool
interacting = \case
  YaleLam{} -> True
  YaleApp{} -> True
  Copy{} -> True
  Delta{} -> True
  Erase{} -> True
  EndList{} -> True
  FreeVar{} -> True
  FreeVarBlocked{} -> True
  Garbage{} -> True
  Constant{} -> True
  Forcing{} -> True
  _ -> False

-- | Whether two interacting nodes form a valid interaction pair.
interacts :: YaleNode -> YaleNode -> Bool
interacts va vb =
  isBeta va vb
    || isCopyPair va vb
    || isDeltaPair va vb
    || isErasePair va vb
    || isGarbagePair va vb
    || isForcingPair va vb
 where
  isBeta x y = (isApp x && isLam y) || (isLam x && isApp y)
  -- A 'Copy' meets a term.
  isCopyPair x y = (isCopy x && isTerm y) || (isTerm x && isCopy y)
  -- The δ agent meets ANY interacting agent: it cancels with another δ
  -- (δ×δ) and duplicates every other agent (the δ×α general rule).
  isDeltaPair x y = (isDelta x && interacting y) || (interacting x && isDelta y)
  isErasePair x y =
    (isErase x && (isEnd y || isFreeV y || isFVB y))
      || ((isEnd x || isFreeV x || isFVB x) && isErase y)
  -- Garbage meets ANY interacting agent (Mackie §5: "E interacting with
  -- anything simply erases the agent, and then erases all the net connected
  -- to the auxiliary ports"). 'interacting' (not 'isTerm') is the partner
  -- predicate so that e can also collect Copy/Delta/Erase agents that sit on
  -- an edge mid-collection; Multiplexer/Initiator are excluded automatically
  -- because 'interacting' is False for them.
  isGarbagePair x y = (isGarbage x && interacting y) || (interacting x && isGarbage y)
  -- The forcing agent (4) meets any interacting agent: it passes over terms
  -- without effect, cancels with another forcing agent, and toggles
  -- FreeVar <-> FreeVarBlocked (Mackie §5.1, Fig. 4).
  isForcingPair x y = (isForcing x && interacting y) || (interacting x && isForcing y)
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
  isDelta (Delta{}) = True
  isDelta _ = False
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
  isForcing (Forcing{}) = True
  isForcing _ = False
