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

Rule-figure cross-reference (fast check against @YALE_by_hands/processed/@):

* 'betaRHS' — rule_01 (@@×λ@)
* 'eraseRHS' — rule_02 (d×ν)
* rule_03 (b×k) — inline dispatch: the @b@ is deleted, the constant remains
* rule_05 (b×ν) — inline dispatch: the @b@ is deleted, the end-list remains
* 'copyConstRHS' — rule_06 (c×k)
* 'copyLamRHS' — rule_07 (c×λ)
* 'copyEndRHS' — rule_08 (c×ν)
* 'deltaDeltaRHS' — rule_09 (δ×δ)
* 'deltaFreeVarRHS' — rule_10 (δ×b)
* 'deltaFreeVarBlockedRHS' — rule_11 (δ×b′)
* 'deltaAlphaRHS' — rule_12 (δ×α)
* 'freeVarFreeVarBlockedRHS' — rule_13 (b×b′)
* 'copyFreeVarBlockedRHS' — rule_14 (c×b′)
* 'eraseFreeVarBlockedRHS' — rule_15 (d×b′)
* 'garbageRHS' — rule_16 (ε×λ)

Not among the 16 extracted figures: 'copyFreeVarRHS' (c×b), 'deltaAppRHS'
(δ×App), 'forcingFreeVarRHS'\/'forcingFreeVarBlockedRHS' (4×b\/4×b′, Mackie
§5.1). Present in the 16 figures but not yet given a dedicated RHS: rule_04
(b×λ) — the @b@ currently passes through the @λ@ by coexisting on the shared
edge (no explicit splice rule).

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
  byEdge,
  byNode,
  byWire,
  deleteNode,
  inspect,
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
      ]

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
  principalPort va == principalPort vb && not (isDeltaApp va vb)
    || agentAtAppResult va vb
    || agentAtAppResult vb va
    || agentAtFvHead va vb
    || agentAtFvHead vb va
 where
  -- A 'Delta' must NOT meet a 'YaleApp' at the application's @appFunc@
  -- (the application's principal port). A @δ@ only ever sits on an
  -- application's @appFunc@ edge when that function is a free-variable
  -- occurrence — i.e. the @fvOcc@ principal of a blocked @b'@ agent. In that
  -- case the @δ@ must unblock the @b'@ (rule_11, @δ@×@b'@), NOT duplicate the
  -- application. Allowing the principal-port @δ@×App@ meeting here lets
  -- 'deltaAppRHS' consume the @δ@ before it can unblock the @b'@, stranding
  -- the @b'@ with an empty @fvOcc@ and halting the reduction (this is why
  -- every @if@ collapsed to a constant). The @δ@ is still free to meet the
  -- application at its @appResult@ port via 'agentAtAppResult' (the body-
  -- duplication case), which is the only legitimate @δ@×App@ meeting.
  isDeltaApp x y = (isDeltaN x && isAppN y) || (isAppN x && isDeltaN y)
  isDeltaN (Delta{}) = True
  isDeltaN _ = False
  isAppN (YaleApp{}) = True
  isAppN _ = False
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
  -- A Forcing agent may meet a FreeVarBlocked (b') at its @fvHead@ port, which
  -- is an auxiliary port of the b'. The b' principal is @fvOcc@; the @fvHead@
  -- port is wired to the forcing agent's primary when the b' was created by the
  -- forcing toggle (4×b). The toggle-back (4×b') absorbs the forcing agent and
  -- converts the b' back to a b, so no new agent is spawned on @fvHead@ and the
  -- pair is well-formed.
  --
  -- NOTE: a 'Delta' must NOT be allowed to meet a b' at @fvHead@. The unblocking
  -- rule (δ×b', rule_11) meets the b' at its @fvOcc@ principal and spawns a fresh
  -- δ on @fvHead@ (the head-duplicator). If a δ already sitting on @fvHead@ (the
  -- one preserved by the blocking rule δ×b, rule_10) were allowed to fire rule_11
  -- directly, the spawned head-δ would share its input edge with the LHS δ;
  -- deleting the LHS δ would strand the head-δ with a dangling input and the
  -- reduction would halt. Instead the preserved δ waits on @fvHead@ until a
  -- different δ reaches the b' @fvOcc@ principal and fires rule_11, which spawns
  -- the partner head-δ that cancels with it (δ×δ).
  agentAtFvHead a@(Forcing{}) (FreeVarBlocked{fvHead = h}) = principalPort a == h
  agentAtFvHead _ _ = False

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
    -- d × b' (rule 15): uses 'rewrite' so the RHS can call 'mergeEdges'
    (TErase, TFVB) -> rewrite $ \_ -> eraseFreeVarBlockedRHS va vb a b
    (TFVB, TErase) -> rewrite $ \_ -> eraseFreeVarBlockedRHS vb va b a
    -- d × b (rule 2): uses 'rewrite' so the RHS can call 'mergeEdges' and
    -- 'newNode' to wire the value to the occurrence and continue the erasure
    -- along the list continuation.
    (TErase, TFreeV) -> rewrite $ \_ -> eraseFreeVarRHS va vb a b
    (TFreeV, TErase) -> rewrite $ \_ -> eraseFreeVarRHS vb va b a
    -- b × κ (rule_03): the b agent is erased; the Constant remains.
    (TFreeV, TConst) -> rewrite $ \_ -> deleteNode a
    (TConst, TFreeV) -> rewrite $ \_ -> deleteNode b
    -- b × ν (rule_05): the b agent is erased; the EndList remains.
    (TFreeV, TEnd) -> rewrite $ \_ -> deleteNode a
    (TEnd, TFreeV) -> rewrite $ \_ -> deleteNode b
    -- b × b' (rule_13): uses 'rewrite' so the RHS can delete the LHS nodes
    -- explicitly (the two b' agents share an internal edge, which the
    -- 'replace' action mishandles).
    (TFreeV, TFVB) -> rewrite $ \_ -> freeVarFreeVarBlockedRHS va vb a b
    (TFVB, TFreeV) -> rewrite $ \_ -> freeVarFreeVarBlockedRHS vb va b a
    _ ->
      replace $ case (tag va, tag vb) of
        -- Copy: c × X
        (TCopy, TCopy) -> copyCopyRHS va vb
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
        -- δ × b / δ × b' (Mackie 1998, §5 "Duplication", rules 2 & 3): the
        -- blocking / unblocking rules. These keep the δ agents well-balanced
        -- on the free-variable list instead of letting them enter the body.
        (TDelta, TFreeV) -> deltaFreeVarRHS va vb
        (TFreeV, TDelta) -> deltaFreeVarRHS vb va
        (TDelta, TFVB) -> deltaFreeVarBlockedRHS va vb
        (TFVB, TDelta) -> deltaFreeVarBlockedRHS vb va
        -- δ × λ: duplicate the abstraction, propagating a c (not a δ) along
        -- the free-variable list to keep the δ agents well-balanced in the body.
        (TDelta, TLam) -> deltaLamRHS va vb
        (TLam, TDelta) -> deltaLamRHS vb va
        (TDelta, _) -> deltaAlphaRHS va vb
        (_, TDelta) -> deltaAlphaRHS vb va
        -- Erase: d × X
        (TErase, TEnd) -> eraseRHS
        (TEnd, TErase) -> eraseRHS
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
Figure: @YALE_by_hands/processed/rule_01.svg@.
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
Figure: @YALE_by_hands/processed/rule_07.svg@.
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
-- Figure: @YALE_by_hands/processed/rule_06.svg@.
copyConstRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyConstRHS c k = do
  byNode $ Constant{inp = copyOut1 c, name = name k}
  byNode $ Constant{inp = copyOut2 c, name = name k}

{- | 'Copy'×'Copy': two @c@ agents meet on the same edge, both duplicating the
same term. The duplication is redundant, so the two @c@ agents are removed and
the term is fanned out to the UNION of the two @c@ agents' four outputs (via a
'Copy' chain, 'createCopyChainReplace'). This preserves every copy the two
@c@ agents were producing, instead of dropping the outputs (as a bare
cancellation would).
NOTE: no extracted figure — c×c is not among the 16 rules in
@YALE_by_hands/processed/@.
-}
copyCopyRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyCopyRHS c1 c2 =
  createCopyChainReplace [] [] (copyIn c1)
    [ copyOut1 c1
    , copyOut2 c1
    , copyOut1 c2
    , copyOut2 c2
    ]

-- | 'Copy'×'EndList': copy the end-of-list. Two @ν@'s.
-- Figure: @YALE_by_hands/processed/rule_08.svg@.
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
NOTE: no extracted figure — c×b is not among the 16 rules in
@YALE_by_hands/processed/@ (the paper's c-meets-free-variable case is rule 14,
c×b′, see 'copyFreeVarBlockedRHS').
-}
copyFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarRHS c b = do
  -- δ for the continuation (b.fvCont)
  eCont1 <- byEdge
  eCont2 <- byEdge
  byNode $ Delta{deltaIn = fvCont b, deltaOut1 = eCont1, deltaOut2 = eCont2}
  -- c for the value (b.fvValue)
  eVal1 <- byEdge
  eVal2 <- byEdge
  byNode $
    Copy
      { copyIn = fvValue b
      , copyOut1 = eVal1
      , copyOut2 = eVal2
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }
  -- c for the occurrence (b.fvOcc)
  eOcc1 <- byEdge
  eOcc2 <- byEdge
  byNode $
    Copy
      { copyIn = fvOcc b
      , copyOut1 = eOcc1
      , copyOut2 = eOcc2
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }
  -- Two b copies; the meeting port (principal) receives the c outputs.
  byNode $ FreeVar{fvPrimary = copyOut1 c, fvOcc = eOcc1, fvCont = eCont1, fvValue = eVal1, name = name b}
  byNode $ FreeVar{fvPrimary = copyOut2 c, fvOcc = eOcc2, fvCont = eCont2, fvValue = eVal2, name = name b}

{- | 'Copy'×'FreeVarBlocked': the blocking rule. A @c@ agent meeting a @b'@
agent converts the @b'@ back to a @b@ (Mackie 1998: "in the process of
duplication we also convert the b' agent back to a b agent"), duplicates it
(two @b@ copies, each with its @fvOcc@ routed by 'routeFvOcc'), and propagates
a @c@ agent along the list continuation (the tail).
Figure: @YALE_by_hands/processed/rule_14.svg@.
-}
copyFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyFreeVarBlockedRHS c b = do
  -- δ for the continuation (b'.fvCont)
  eCont1 <- byEdge
  eCont2 <- byEdge
  byNode $ Delta{deltaIn = fvCont b, deltaOut1 = eCont1, deltaOut2 = eCont2}
  -- c for the value (b'.fvValue)
  eVal1 <- byEdge
  eVal2 <- byEdge
  byNode $
    Copy
      { copyIn = fvValue b
      , copyOut1 = eVal1
      , copyOut2 = eVal2
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }
  -- c for the head (b'.fvHead)
  eHead1 <- byEdge
  eHead2 <- byEdge
  byNode $
    Copy
      { copyIn = fvHead b
      , copyOut1 = eHead1
      , copyOut2 = eHead2
      , copySticky = copySticky c
      , copySplitVars = copySplitVars c
      }
  -- Two b' copies; the occurrence (principal) receives the c outputs.
  byNode $ FreeVarBlocked{fvOcc = copyOut1 c, fvCont = eCont1, fvValue = eVal1, fvHead = eHead1, name = name b}
  byNode $ FreeVarBlocked{fvOcc = copyOut2 c, fvCont = eCont2, fvValue = eVal2, fvHead = eHead2, name = name b}

{- | 'Delta'×'Delta': two @δ@ agents meet and cancel (Mackie 1998, §5
"Duplication"). This indicates that the duplication process is complete. The
four auxiliary ports are reconnected so that each @δ@'s @deltaOut1@ meets the
other @δ@'s @deltaOut1@ and each @deltaOut2@ meets the other @deltaOut2@
(parallel, NOT crossed): @out1@ and @out2@ consistently carry "copy 1" and
"copy 2" respectively (see 'copyLamRHS' and 'deltaAlphaRHS'), so the
cancellation must preserve that distinction or the two copies' sub-nets get
swapped and the data flow is scrambled.
Figure: @YALE_by_hands/processed/rule_09.svg@.
-}
deltaDeltaRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaDeltaRHS d1 d2 =
  byWire (deltaOut1 d1) (deltaOut1 d2)
    <> byWire (deltaOut2 d1) (deltaOut2 d2)

{- | 'Delta'×'FreeVar': the BLOCKING rule (Mackie 1998, §5 "Duplication",
second rule). A @δ@ agent meeting a @b@ agent does NOT duplicate it; instead
the @b@ is converted to a @b'@ (blocked free-variable) agent, whose new
principal port (@fvHead@) is wired to the interaction edge (the @δ@'s former
input). The @δ@ is preserved (not absorbed), so the pair immediately re-fires
as @δ@×@b'@ (the unblocking rule below). The point of the block is that the
@b'@'s new @fvHead@ port lets OTHER interactions complete while the @δ@ stays
on the free-variable list — it is not allowed to enter the body through the
occurrence, keeping the @δ@ agents well-balanced.
Figure: @YALE_by_hands/processed/rule_10.svg@.
-}
deltaFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaFreeVarRHS d b = do
  -- The b is converted to a b': the occurrence becomes the principal, the
  -- continuation and value are preserved, and the meeting port (the δ's
  -- former input) becomes the new head port. The δ is preserved, its primary
  -- now wired to the b' head.
  byNode $ FreeVarBlocked{fvOcc = fvOcc b, fvCont = fvCont b, fvValue = fvValue b, fvHead = deltaIn d, name = name b}
  byNode $ Delta{deltaIn = deltaIn d, deltaOut1 = deltaOut1 d, deltaOut2 = deltaOut2 d}

{- | 'Delta'×'FreeVarBlocked': the UNBLOCKING rule (Mackie 1998, §5
"Duplication", third rule). A @δ@ agent meeting a @b'@ agent converts the @b'@
back to a @b@ and duplicates it into two @b@ copies (each copy's principal
@fvCont@ wired to a @δ@ output). A fresh @δ@ is propagated along the list
continuation (@fvCont@, "along the list of free variables") AND along the
occurrence edge (@fvOcc@, "along the free variable edge"), so the duplication
continues down the list and into the occurrence. Unlike the generic @δ@×@α@
rule (which would duplicate the @b'@ and propagate only along its auxiliary
ports), this rule unblocks the @b'@ and keeps the @δ@ balanced on the list.
Figure: @YALE_by_hands/processed/rule_11.svg@.
-}
deltaFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaFreeVarBlockedRHS d b = do
  -- δ_top: duplicates the continuation (b'.fvCont)
  eCont1 <- byEdge
  eCont2 <- byEdge
  byNode $ Delta{deltaIn = fvCont b, deltaOut1 = eCont1, deltaOut2 = eCont2}
  -- δ_mid: duplicates the value (b'.fvValue); its outputs feed b1's value
  -- and meeting ports.
  eVal1 <- byEdge
  eMeet1 <- byEdge
  byNode $ Delta{deltaIn = fvValue b, deltaOut1 = eVal1, deltaOut2 = eMeet1}
  -- δ_bot: duplicates the head (b'.fvHead); its outputs feed b2's value and
  -- meeting ports.
  eVal2 <- byEdge
  eMeet2 <- byEdge
  byNode $ Delta{deltaIn = fvHead b, deltaOut1 = eVal2, deltaOut2 = eMeet2}
  -- Two b copies; the occurrence (the δ's outputs) is the principal's
  -- counterpart, the meeting port is fed by the value/head δ.
  byNode $ FreeVar{fvPrimary = eMeet1, fvOcc = deltaOut1 d, fvCont = eCont1, fvValue = eVal1, name = name b}
  byNode $ FreeVar{fvPrimary = eMeet2, fvOcc = deltaOut2 d, fvCont = eCont2, fvValue = eVal2, name = name b}

{- | 'FreeVar'×'FreeVarBlocked': the @b@ and @b'@ agents meet and reconfigure
into two @b'@ agents and one @b@ agent (Mackie 1998, §5, rule 13). The @b@'s
continuation becomes the top @b'@'s occurrence; the @b'@'s continuation becomes
the top @b'@'s continuation; the @b@'s occurrence becomes the bottom @b'@'s
occurrence; the @b'@'s head becomes the bottom @b'@'s head; the @b@'s value
becomes the @b@'s value; the @b'@'s value becomes the @b@'s meeting port. The
three internal edges wire the value\/head\/continuation ports across the agents.
Figure: @YALE_by_hands/processed/rule_13.svg@.
-}
freeVarFreeVarBlockedRHS :: YaleNode -> YaleNode -> Node -> Node -> Rewrite YaleNode ()
freeVarFreeVarBlockedRHS b bprime bNode bprimeNode = do
  -- Capture the LHS edges before deleting the LHS nodes.
  let eContB = fvCont b
      eOccB = fvOcc b
      eValB = fvValue b
      eContP = fvCont bprime
      eValP = fvValue bprime
      eHeadP = fvHead bprime
  -- Delete the LHS nodes FIRST so their edges are freed; the new nodes then
  -- attach to the freed edges (not to edges still shared with live LHS nodes).
  deleteNode bNode
  deleteNode bprimeNode
  eA <- newEdge -- top b'.fvValue = b.fvCont
  eB <- newEdge -- top b'.fvHead = bottom b'.fvCont
  eC <- newEdge -- bottom b'.fvValue = b.fvOcc
  void $ newNode $ FreeVarBlocked{fvOcc = eContB, fvCont = eContP, fvValue = eA, fvHead = eB, name = name b}
  void $ newNode $ FreeVarBlocked{fvOcc = eOccB, fvCont = eB, fvValue = eC, fvHead = eHeadP, name = name b}
  void $ newNode $ FreeVar{fvPrimary = eValP, fvOcc = eC, fvCont = eA, fvValue = eValB, name = name b}

{- | 'Delta'×'YaleApp': the @δ@ agent duplicates an application. Because the
application's principal is @appFunc@ (so that β-reduction is a principal-port
interaction), the @δ@ — which is spawned on the body\/result edge — meets the
application at its @appResult@ port (via the @appResult@ meeting exception),
NOT at the principal. The meeting port must be the one wired to the @δ@'s two
outputs; the other two ports each receive a propagated @δ@. Wiring the wrong
port (as the generic @δ@×@α@ rule would) disconnects the function and feeds the
duplication back into itself, growing the net without bound.
NOTE: no extracted figure — δ×App is a codebase-specific special case (the
application's principal is @appFunc@, so the @δ@ meets it at @appResult@); it
is not among the 16 rules in @YALE_by_hands/processed/@.
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

{- | 'Delta'×'YaleLam': the @δ@ agent duplicates an abstraction. This is the
δ-analogue of the @c@×@λ@ rule ('copyLamRHS'): two copies of the @λ@ are
produced, a @δ@ is introduced to duplicate the binder edge and the body, and a
@c@ (NOT a @δ@) is propagated along the free-variable list. Propagating a @c@
along the list — rather than a @δ@, as the generic 'deltaAlphaRHS' would — is
what keeps the @δ@ agents well-balanced inside the body of the abstraction
(Mackie 1998, §5 "Duplication"): a @δ@ on the free-variable list would enter
the @c@-chains that fan captured values to their occurrences and duplicate
them, re-entering nested abstractions without bound. The two copies' principal
ports are wired to the @δ@'s two outputs (as in 'deltaAlphaRHS').
NOTE: no extracted figure — δ×λ is the δ-analogue of rule 7 (c×λ); it is not
among the 16 rules in @YALE_by_hands/processed/@.
-}
deltaLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
deltaLamRHS d lam = do
  let v = lamVar lam
  let bodyEdge = lamBody lam
  v1 <- byEdge
  v2 <- byEdge
  eList1 <- byEdge
  eList2 <- byEdge
  eBody1 <- byEdge
  eBody2 <- byEdge
  byNode $ YaleLam{lamResult = deltaOut1 d, lamVarList = eList1, lamVar = v1, lamBody = eBody1}
  byNode $ YaleLam{lamResult = deltaOut2 d, lamVarList = eList2, lamVar = v2, lamBody = eBody2}
  byNode $ Delta{deltaIn = v, deltaOut1 = v1, deltaOut2 = v2}
  byNode $ Delta{deltaIn = bodyEdge, deltaOut1 = eBody1, deltaOut2 = eBody2}
  byNode $
    Copy{copyIn = lamVarList lam, copyOut1 = eList1, copyOut2 = eList2, copySticky = [], copySplitVars = []}

{- | 'Delta'×@α@: the general duplication rule (Mackie 1998, §5 "Duplication",
final rule scheme). The @δ@ agent duplicates the agent @α@ into two copies and
propagates a fresh @δ@ agent along every auxiliary port of @α@, so that the
duplication continues into the sub-net. The two copies' principal ports are
wired to the @δ@'s two outputs. This is a pure local replacement with no
metadata.
Figure: @YALE_by_hands/processed/rule_12.svg@.
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

-- | 'Erase'×'EndList': the @d@ agent meets the end of the list. Both agents
-- are deleted (the erasure is complete).
-- Figure: @YALE_by_hands/processed/rule_02.svg@ (d×ν).
eraseRHS :: Replace YaleNode ()
eraseRHS = return ()

{- | 'Erase'×'FreeVar': the @d@ agent meets a @b@ agent (Mackie 1998, §5,
rule 2). The @b@ agent is erased: its @fvValue@ (wired to the enclosing binder
edge) is merged onto its @fvOcc@ (the occurrence in the body), so the value
flows to the occurrence. The @d@ agent is re-introduced on the list
continuation (@fvCont@) to continue walking the list.
-}
eraseFreeVarRHS :: YaleNode -> YaleNode -> Node -> Node -> Rewrite YaleNode ()
eraseFreeVarRHS _ b dNode bNode = do
  -- Merge the value onto the occurrence FIRST (while the b agent still exists,
  -- so the edges are valid), then create the continuation d agent, then delete
  -- both agents.
  mergeEdges (fvValue b) (fvOcc b)
  void $ newNode Erase{eraseIn = fvCont b}
  deleteNode dNode
  deleteNode bNode

{- | 'Erase'×'FreeVarBlocked': the @d@ agent meets a @b'@ (Mackie 1998, §5,
rule 15). The @d@ agent is PRESERVED (not deleted): its @eraseIn@ is rewired
to the @b'@'s value port, so the erasure continues along the value. The
@b'@'s continuation and head ports are merged (they no longer dangle).
Figure: @YALE_by_hands/processed/rule_15.svg@.
-}
eraseFreeVarBlockedRHS :: YaleNode -> YaleNode -> Node -> Node -> Rewrite YaleNode ()
eraseFreeVarBlockedRHS _ b dNode bNode = do
  deleteNode dNode
  deleteNode bNode
  mergeEdges (fvCont b) (fvHead b)
  void $ newNode Erase{eraseIn = fvValue b}

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
Figure: @YALE_by_hands/processed/rule_16.svg@ (ε×λ; the generic ε×α case).
-}
garbageRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
garbageRHS _ x = case x of
  YaleLam{} -> spawnGarbageOn [lamVarList x, lamBody x]
  YaleApp{} -> spawnGarbageOn [appArg x, appResult x]
  Copy{} -> spawnGarbageOn [copyOut1 x, copyOut2 x]
  Delta{} -> spawnGarbageOn [deltaOut1 x, deltaOut2 x]
  FreeVar{} -> spawnGarbageOn [fvOcc x, fvCont x, fvValue x]
  FreeVarBlocked{} -> spawnGarbageOn [fvCont x, fvValue x, fvHead x]
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
NOTE: no extracted figure — the Forcing (4) agent is from Mackie §5.1
"Strong Reduction" (Fig. 4), not among the 16 rules in
@YALE_by_hands/processed/@.
-}
forcingFreeVarRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
forcingFreeVarRHS f b =
  byNode $ FreeVarBlocked{fvOcc = fvOcc b, fvCont = fvCont b, fvValue = fvValue b, fvHead = forcingIn f, name = name b}

{- | 'Forcing'×'FreeVarBlocked': convert the @b'@ agent back to a @b@ agent.
The forcing agent is absorbed.
NOTE: no extracted figure — the Forcing (4) agent is from Mackie §5.1
"Strong Reduction" (Fig. 4), not among the 16 rules in
@YALE_by_hands/processed/@.
-}
forcingFreeVarBlockedRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
forcingFreeVarBlockedRHS f b =
  byNode $ FreeVar{fvPrimary = forcingIn f, fvOcc = fvOcc b, fvCont = fvCont b, fvValue = fvValue b, name = name b}

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
    || isFreeVPair va vb
    || isFreeVFreeVBPair va vb
    || isGarbagePair va vb
    || isForcingPair va vb
 where
  -- b × κ (rule_03) and b × ν (rule_05): the b agent meets a Constant or
  -- EndList at its principal port and is erased; the other agent remains.
  isFreeVPair x y =
    (isFreeV x && (isConstant y || isEnd y))
      || ((isConstant x || isEnd x) && isFreeV y)
  -- b × b' (rule_13): the b and b' agents meet and reconfigure into two b'
  -- agents and one b agent.
  isFreeVFreeVBPair x y = (isFreeV x && isFVB y) || (isFVB x && isFreeV y)
  isConstant (Constant{}) = True
  isConstant _ = False
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
