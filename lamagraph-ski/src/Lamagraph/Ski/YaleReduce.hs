{- | A step-bounded reduction driver for the full YALE (Mackie 1998) lambda
interaction net.

The interactions (all principal-port meetings):

* β-reduction — a 'YaleApp' (application) met at its principal (function) port
  by a 'YaleLam' (abstraction) met at its principal (result) port: both are
  deleted, the abstraction's body is wired to the application's result, the
  application's argument is wired to the abstraction's bound-variable port
  (@lamVar@), and an 'Erase' (@d@) agent is introduced at the head of the
  abstraction's free-variable list.
* 'Erase'×'EndList' — the @d@ agent erases the end-of-list @ν@ (the list is
  empty; the erasure is complete).
* 'Copy'×'YaleLam' — the @c@ agent copies the abstraction: two copies of the
  @λ@ are produced, a 'Subst' (@s@) agent is propagated into each copy's body,
  and a 'Copy' is propagated along each copy's free-variable list.
* 'Copy'×'YaleApp' — the @c@ agent copies the application: two copies of the
  @@@ are produced, and a 'Copy' is propagated along each copy's auxiliary
  ports.
* 'Copy'×'Constant' — the @c@ agent copies a base value: two copies.
* 'Copy'×'EndList' — the @c@ agent copies the end-of-list: two @ν@'s.
* 'Subst'×'YaleApp' — the @s@ agent propagates into the application's function
  and argument.
* 'Subst'×'YaleLam' — the @s@ agent propagates into the abstraction's body.
* 'Subst'×'Constant' — the @s@ agent is erased at a base value.
* 'Garbage'×(any term) — the @e@ agent erases the term and the net on its
  auxiliary ports.

The driver is a worklist: at each step it picks one active pair (two
interacting nodes joined at their principal ports) and rewrites it, until no
active pair remains (normal form) or the step budget is exhausted. The firing
order is arbitrary (the first active pair in node order).
-}
module Lamagraph.Ski.YaleReduce (
  reduceStepsYale,
) where

import Relude

import GraphRewriting (
  Graph,
  Node,
  Replace,
  Rule,
  apply',
  byConnector,
  byEdge,
  byNode,
  nodeAt,
  readNode,
  readNodeList,
  replace,
  require,
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

{- | All active pairs of the net: unordered pairs of distinct interacting nodes
joined at their principal (port 0) edge, where the pair actually interacts.
The order is deterministic (node order) but arbitrary with respect to the
reduction.
-}
findActivePairs :: Graph YaleNode -> [(Node, Node)]
findActivePairs = runReader go
 where
  go :: Reader (Graph YaleNode) [(Node, Node)]
  go = do
    ns <- readNodeList
    vs <- mapM readNode ns
    return
      [ (a, b)
      | (a, va) <- zip ns vs
      , interacting va
      , (b, vb) <- zip ns vs
      , b > a
      , interacting vb
      , principalPort va == principalPort vb
      , interacts va vb
      ]

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
  let pa = principalPort va
      pb = principalPort vb
  require (pa == pb)
  replace $ case (tag va, tag vb) of
    -- β-reduction: @ × λ
    (TApp, TLam) -> betaRHS va vb
    (TLam, TApp) -> betaRHS vb va
    -- Copy: c × X
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
    -- Erase: d × X
    (TErase, TEnd) -> eraseRHS
    (TEnd, TErase) -> eraseRHS
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
-}
betaRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
betaRHS app lam = do
  eD <- byEdge
  byNode $ Erase{eraseIn = eD}
  byConnector [lamBody lam, appResult app]
  byConnector [appArg app, lamVar lam]
  byConnector [eD, lamVarList lam]

{- | 'Copy'×'YaleLam': copy the abstraction. Two copies of the @λ@ are
produced; a 'Subst' (@s@) agent is propagated into each copy's body; a
'Copy' (@c@) agent is propagated along each copy's free-variable list.
-}
copyLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyLamRHS c lam = do
  eBody1 <- byEdge
  eBody2 <- byEdge
  eVar1 <- byEdge
  eVar2 <- byEdge
  eList1 <- byEdge
  eList2 <- byEdge
  eB <- byEdge
  byNode $ YaleLam{lamResult = copyOut1 c, lamVarList = eList1, lamVar = eVar1, lamBody = eBody1}
  byNode $ YaleLam{lamResult = copyOut2 c, lamVarList = eList2, lamVar = eVar2, lamBody = eBody2}
  -- A c agent is introduced to duplicate the body (Mackie 1998: "a c agent is
  -- introduced to duplicate the body of the abstraction, in the same way as we
  -- copied the abstraction"). The body is duplicated, not shared: each copy
  -- gets its own body net. (s agents for nested abstractions are created
  -- recursively when the c agent walks into the body and meets a nested λ.)
  byNode $ Copy{copyIn = lamBody lam, copyOut1 = eBody1, copyOut2 = eBody2}
  -- A b' (blocked free-variable) agent is introduced at the head of the
  -- free-variable list (Mackie 1998: "the blocking is achieved by introducing
  -- a new agent b'"). The @c@ agent must meet the b' FIRST (not the ν that
  -- terminates the original list). The b' therefore sits on a fresh head edge
  -- @eHead@ together with the @c@ agent (via its principal @fvHead@ port); its
  -- @fvCont@ continuation points at the original list, so after c×b' fires the
  -- propagated @c@ reaches the original list (ν) and duplicates the rest.
  eHead <- byEdge
  byNode $ FreeVarBlocked{fvHead = eHead, fvCont = lamVarList lam, fvOcc = eB, name = ""}
  byNode $ Copy{copyIn = eHead, copyOut1 = eList1, copyOut2 = eList2}
  -- The bound-variable occurrence is shared by both copies (the s agents in
  -- the bodies detach it as they walk).
  byConnector [eVar1, lamVar lam]
  byConnector [eVar2, lamVar lam]

{- | 'Copy'×'YaleApp': copy the application. Two copies of the @@@ are
produced; a 'Copy' (@c@) agent is propagated along each copy's auxiliary
ports (the function and the argument).
-}
copyAppRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
copyAppRHS c app = do
  eFunc1 <- byEdge
  eFunc2 <- byEdge
  eArg1 <- byEdge
  eArg2 <- byEdge
  byNode $ YaleApp{appFunc = eFunc1, appArg = eArg1, appResult = copyOut1 c}
  byNode $ YaleApp{appFunc = eFunc2, appArg = eArg2, appResult = copyOut2 c}
  -- c is propagated along the function and argument ports: each new c agent
  -- sits on the original port edge and fans it out to the two copies.
  byNode $ Copy{copyIn = appFunc app, copyOut1 = eFunc1, copyOut2 = eFunc2}
  byNode $ Copy{copyIn = appArg app, copyOut1 = eArg1, copyOut2 = eArg2}

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
  byNode $ Copy{copyIn = fvCont b, copyOut1 = copyOut1 c, copyOut2 = copyOut2 c}

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
  byNode $ Copy{copyIn = fvCont b, copyOut1 = copyOut1 c, copyOut2 = copyOut2 c}

{- | 'Subst'×'YaleApp': the @s@ agent propagates into the application's
function and argument. The application is preserved (recreated) with its
result rewired to the @s@ output.
-}
substAppRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substAppRHS s app = do
  eS2 <- byEdge
  -- The recreated @@@ reuses the meeting edge (the @s@ agent's principal) as
  -- its principal port, so a second @s@ agent waiting on that edge (from
  -- 'Copy'×'YaleLam') can continue after this one fires.
  byNode $ YaleApp{appFunc = substIn s, appArg = eS2, appResult = substOut s}
  byNode $ Subst{substIn = appFunc app, substOut = substIn s}
  byNode $ Subst{substIn = appArg app, substOut = eS2}

{- | 'Subst'×'YaleLam': the @s@ agent propagates into the abstraction's body.
The result, bound variable, and free-variable list are preserved.
-}
substLamRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substLamRHS s lam = do
  eS <- byEdge
  byNode $ YaleLam{lamResult = substOut s, lamVarList = lamVarList lam, lamVar = lamVar lam, lamBody = eS}
  byNode $ Subst{substIn = lamBody lam, substOut = eS}

{- | 'Subst'×'Constant': the @s@ agent is erased at a base value; the base
value is preserved (recreated) with its input rewired to the @s@ output.
-}
substConstRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
substConstRHS s k = do
  byNode $ Constant{inp = substOut s, name = name k}

-- | 'Erase'×(EndList\/FreeVarBlocked): the @d@ agent erases the list element.
eraseRHS :: Replace YaleNode ()
eraseRHS = return ()

{- | 'Garbage'×(any term): the @e@ agent erases the term and the net on its
auxiliary ports.
-}
garbageRHS :: YaleNode -> YaleNode -> Replace YaleNode ()
garbageRHS _ _ = return ()

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
  | TGarbage
  deriving (Show)

-- | The tag of a node.
tag :: YaleNode -> Tag
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
  isCopyPair x y = (isCopy x && isTerm y) || (isTerm x && isCopy y)
  isSubstPair x y = (isSubst x && isTerm y) || (isTerm x && isSubst y)
  isErasePair x y =
    (isErase x && (isEnd y || isFVB y))
      || ((isEnd x || isFVB x) && isErase y)
  isGarbagePair x y = (isGarbage x && isTerm y) || (isTerm x && isGarbage y)
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
  isErase (Erase{}) = True
  isErase _ = False
  isEnd (EndList{}) = True
  isEnd _ = False
  isFVB (FreeVarBlocked{}) = True
  isFVB _ = False
  isGarbage (Garbage{}) = True
  isGarbage _ = False
