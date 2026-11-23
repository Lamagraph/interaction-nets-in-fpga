{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | A lambda-calculus interaction net using the full YALE (Mackie 1998)
signature and dynamics.

YALE separates abstraction and application into distinct agents, so that
β-reduction SPLICES the argument onto the bound-variable occurrence rather than
aux-zipping two dual ports. It also carries the full duplication\/erasure
machinery of the paper:

* 'YaleLam' — an abstraction @λx. M@. Principal = @lamResult@ (the result).
  Auxiliary ports: @lamVarList@ (the free-variable list, a chain of 'FreeVar'
  agents ending in an 'EndList'), @lamVar@ (the bound-variable occurrence), and
  @lamBody@ (the body net).
* 'YaleApp' — an application @M N@. Principal = @appFunc@ (the function); the
  argument is @appArg@ and the result is @appResult@. (The principal is the
  function port so that the β-rule is a principal-port interaction: the
  function of an application meets the result of an abstraction.)
* 'Copy' — the @c@ (copy) agent, arity 2. Principal = @copyIn@ (the term to
  copy); @copyOut1@\/@copyOut2@ are the two copies. Meets a term and produces
  two copies; on an abstraction it propagates a 'Copy' along the free-variable
  list and introduces 'Delta' agents to duplicate the body.
* 'Delta' — the @δ@ (delta) agent, the "working" duplication agent. Principal =
  @deltaIn@ (the sub-net to duplicate); @deltaOut1@\/@deltaOut2@ are the two
  copies. Walks through a sub-net duplicating every agent it meets, blocks at
  free-variable agents, and cancels when two @δ@ agents meet.
* 'Erase' — the @d@ agent. Introduced at the head of a free-variable list by
  the β-rule; it walks the list and erases it ('Erase'×'EndList',
  'Erase'×'FreeVarBlocked').
* 'FreeVar'\/'FreeVarBlocked' — the @b@\/@b'@ agents, one per free variable of
  an abstraction. (Absent for closed terms, whose list is just an 'EndList'.)
* 'EndList' — the @ν@ agent, the end of a free-variable list.
* 'Garbage' — the @e@ agent; erases whatever it meets and the net on the
  auxiliary ports (garbage collection).
* 'Constant' — a base value \/ named marker (arity 0).
* 'Multiplexer' — a compile-time-only binder, compiled away by
  'compileBinder'.

Recursion ('Lamagraph.Ski.Lambda.LRec') is encoded as a CYCLIC net using the
YALE "tie the knot" technique (Mackie 1998, Fig. p.9): two 'YaleApp' nodes,
each fed by its own copy of the net for @F = λself. inner@; the top @ applies
@F@ to the knot edge, producing the output; the bottom @ applies @F@ to the
knot edge, producing the knot edge itself (the self-loop).
-}
module Lamagraph.Ski.YaleNet (
  YaleNode (..),
  buildYaleNet,
  buildYaleNetK,
  BuildM,
  Projection,
  idProj,
  compileBinder,
  compileBinders,
) where

import Relude

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.View
import GraphRewriting (
  Edge,
  Graph,
  Rewrite,
  Rule,
  apply,
  deleteNode,
  emptyGraph,
  execGraph,
  exhaustive,
  mergeEdges,
  modifyNode,
  newEdge,
  newNode,
  node,
  rewrite,
 )
import GraphRewriting.Pattern.InteractionNet (INet (..))

import Lamagraph.Ski.Lambda (Lam (..), fv, replaceSelfRef, selfRefName)

-- * Node type (the YALE signature)

-- | A node of a YALE lambda interaction net.
data YaleNode
  = -- | The single root node; @out@ is the term's output edge.
    Initiator
      { out :: Edge
      }
  | {- | An abstraction @λx. M@. @lamResult@ is the principal (result) port;
    @lamVarList@ is the free-variable list (a 'FreeVar' chain ending in an
    'EndList'); @lamVar@ is the bound-variable occurrence; @lamBody@ is the
    body net.
    -}
    YaleLam
      { lamResult :: Edge
      , lamVarList :: Edge
      , lamVar :: Edge
      , lamBody :: Edge
      }
  | {- | An application @M N@. @appFunc@ is the principal (function) port;
    @appArg@ is the argument; @appResult@ is the result.
    -}
    YaleApp
      { appFunc :: Edge
      , appArg :: Edge
      , appResult :: Edge
      }
  | {- | The @c@ (copy) agent. @copyIn@ is the principal (the term to copy);
    @copyOut1@\/@copyOut2@ are the two copies.

    @copySticky@ is METADATA (not a port): the list of enclosing binder edges
    that must be PRESERVED when duplicating the term's sub-nets. A port that
    sits on one of these edges is a bound-variable occurrence of an OUTER
    abstraction; it is shared between the two copies (kept on the binder edge).

    @copySplitVars@ is METADATA (not a port): a list of @(original, v1, v2)@
    triples for binder edges of abstractions being duplicated. When a port sits
    on one of the @original@ edges, it is SPLIT: copy 1 gets @v1@ and copy 2
    gets @v2@ (each copy gets its own binder edge). This is what makes
    λ-duplication correct: the two copies of a λ each get their own bound
    variable edge, so that β-reduction on one copy does not interfere with the
    other.
    -}
    Copy
      { copyIn :: Edge
      , copyOut1 :: Edge
      , copyOut2 :: Edge
      , copySticky :: [Edge]
      , copySplitVars :: [(Edge, Edge, Edge)]
      }
  | {- | The @δ@ (delta) agent, the "working" duplication agent (Mackie 1998,
    §5 "Duplication"). Introduced by the @c@×@λ@ rule to duplicate the body of
    an abstraction ("a δ agent is introduced to duplicate the body of the
    abstraction, in the same way as we copied the abstraction agent").
    @deltaIn@ is the principal (the sub-net to duplicate); @deltaOut1@\/
    @deltaOut2@ are the two copies. The @δ@ agent walks through the sub-net,
    duplicating every agent it meets (the @δ@×@α@ general rule), blocks at a
    free-variable agent (@δ@×@b@ → @b'@), unblocks and duplicates a blocked one
    (@δ@×@b'@ → @b@,@b@), and cancels when two @δ@ agents meet (@δ@×@δ@).
    -}
    Delta
      { deltaIn :: Edge
      , deltaOut1 :: Edge
      , deltaOut2 :: Edge
      }
  | {- | The @d@ (eraser) agent, introduced at the head of a free-variable
    list by the β-rule.
    -}
    Erase
      { eraseIn :: Edge
      }
  | {- | A @b@ (free-variable) agent (Mackie 1998, §5). Four ports:
    @fvPrimary@ is the principal (the meeting port, where @δ@\/@c@\/@d@\/@b'@
    meet it); @fvOcc@ the variable occurrence; @fvCont@ the list continuation
    (the rest of the free-variable list); @fvValue@ the value port (carries the
    value spliced in during β-reduction).
    -}
    FreeVar
      { fvPrimary :: Edge
      , fvOcc :: Edge
      , fvCont :: Edge
      , fvValue :: Edge
      , name :: String
      }
  | {- | A @b'@ (blocked free-variable) agent (Mackie 1998, §5). Four ports:
    @fvOcc@ is the principal (the occurrence port, where @δ@\/@c@\/@d@\/@b@
    meet it); @fvCont@ the list continuation; @fvValue@ the value port;
    @fvHead@ the head port (wired to a @δ@'s primary when created by the
    blocking rule).
    -}
    FreeVarBlocked
      { fvOcc :: Edge
      , fvCont :: Edge
      , fvValue :: Edge
      , fvHead :: Edge
      , name :: String
      }
  | -- | The @ν@ (end-of-list) agent.
    EndList
      { endIn :: Edge
      }
  | -- | The @e@ (garbage) agent; erases whatever it meets.
    Garbage
      { garbageIn :: Edge
      }
  | {- | The @4@ (forcing) agent (Mackie 1998, §5.1 "Strong Reduction", Fig. 4).
    Introduced at the free edges of the net to supply dummy arguments so that
    blocked computations complete (strong reduction). It behaves like an
    identity agent: it passes over every agent without effect, EXCEPT:
    * @4@×@4@ — the two cancel (the forcing is complete);
    * @4@×@b@ — converts the @b@ (free-variable) agent into a @b'@
      (blocked free-variable) agent, which allows other reductions to complete;
    * @4@×@b'@ — converts the @b'@ agent back to a @b@ agent.
    The intuition: "when no further substitutions can be done, the free
    variables of an abstraction are allowed to be duplicated and erased."
    -}
    Forcing
      { forcingIn :: Edge
      }
  | -- | A base value \/ named marker (arity 0).
    Constant
      { inp :: Edge
      , name :: String
      }
  | {- | A compile-time-only binder; @out@ is the binder edge and @ins@ the
    edges of the variable's references (innermost-first). Compiled away by
    'compileBinder'.
    -}
    Multiplexer
      { out :: Edge
      , ins :: [Edge]
      }
  deriving (Show)

-- * View / INet instances

instance View [Edge] YaleNode where
  inspect n = case n of
    Initiator{out = o} -> [o]
    YaleLam{lamResult = a, lamVarList = b, lamVar = c, lamBody = d} -> [a, b, c, d]
    YaleApp{appFunc = a, appArg = b, appResult = c} -> [a, b, c]
    Copy{copyIn = a, copyOut1 = b, copyOut2 = c, copySticky = _, copySplitVars = _} -> [a, b, c]
    Delta{deltaIn = a, deltaOut1 = b, deltaOut2 = c} -> [a, b, c]
    Erase{eraseIn = a} -> [a]
    FreeVar{fvPrimary = a, fvOcc = b, fvCont = c, fvValue = d, name = _} -> [a, b, c, d]
    FreeVarBlocked{fvOcc = a, fvCont = b, fvValue = c, fvHead = d, name = _} -> [a, b, c, d]
    EndList{endIn = a} -> [a]
    Garbage{garbageIn = a} -> [a]
    Forcing{forcingIn = a} -> [a]
    Constant{inp = i} -> [i]
    Multiplexer{out = o, ins = is} -> o : is
  update ports n = case n of
    Initiator{} -> n{out = o}
     where
      [o] = ports
    YaleLam{} -> n{lamResult = a, lamVarList = b, lamVar = c, lamBody = d}
     where
      [a, b, c, d] = ports
    YaleApp{} -> n{appFunc = a, appArg = b, appResult = c}
     where
      [a, b, c] = ports
    Copy{} -> n{copyIn = a, copyOut1 = b, copyOut2 = c}
     where
      [a, b, c] = ports
    Delta{} -> n{deltaIn = a, deltaOut1 = b, deltaOut2 = c}
     where
      [a, b, c] = ports
    Erase{} -> n{eraseIn = a}
     where
      [a] = ports
    FreeVar{} -> n{fvPrimary = a, fvOcc = b, fvCont = c, fvValue = d}
     where
      [a, b, c, d] = ports
    FreeVarBlocked{} -> n{fvOcc = a, fvCont = b, fvValue = c, fvHead = d}
     where
      [a, b, c, d] = ports
    EndList{} -> n{endIn = a}
     where
      [a] = ports
    Garbage{} -> n{garbageIn = a}
     where
      [a] = ports
    Forcing{} -> n{forcingIn = a}
     where
      [a] = ports
    Constant{} -> n{inp = i}
     where
      [i] = ports
    Multiplexer{} -> n{out = o, ins = is}
     where
      o : is = ports

instance INet YaleNode where
  principalPort n = case n of
    Initiator{out = o} -> o
    YaleLam{lamResult = a} -> a
    YaleApp{appFunc = a} -> a
    Copy{copyIn = a} -> a
    Delta{deltaIn = a} -> a
    Erase{eraseIn = a} -> a
    FreeVar{fvPrimary = a} -> a
    FreeVarBlocked{fvOcc = a} -> a
    EndList{endIn = a} -> a
    Garbage{garbageIn = a} -> a
    Forcing{forcingIn = a} -> a
    Constant{inp = i} -> i
    Multiplexer{out = o} -> o

-- * Build monad and binder environment

-- | The build monad: the graph-building 'Rewrite' monad.
type BuildM = Rewrite YaleNode

{- | A bound name: its symbol, its binder edge, and a 'reference' that appends
a fresh input edge to the binder 'Multiplexer' (wiring one occurrence of the
variable).
-}
data Name = Name
  { symbol :: String
  , binderEdge :: Edge
  , reference :: BuildM Edge
  }

-- | The binder environment: a list of @Name@'s, innermost binder first.
type Env = [Name]

{- | Bind a name, creating its binder 'Multiplexer'. Returns the binder edge
and the @Name@ (whose 'reference' wires variable occurrences).
-}
bindName :: String -> BuildM (Edge, Name)
bindName sym = do
  v <- newEdge
  s <- newNode Multiplexer{out = v, ins = []}
  let ref = do
        e <- newEdge
        modifyNode s $ \m -> case m of
          Multiplexer{out = o, ins = is} -> Multiplexer{out = o, ins = e : is}
          -- Unreachable: @s@ is the 'Multiplexer' created above.
          _ -> m
        return e
  return (v, Name{symbol = sym, binderEdge = v, reference = ref})

-- * Free-variable context (b agents)

{- | The free-variable context: a stack of maps (innermost first), where each
map maps a captured variable name to the list of its @c@-chain OUTPUT edges
(one per occurrence, in body order).

Paper-faithful encoding (Mackie 1998, §3 Conv. 3.3 + §4 "Duplication"): a
captured variable occurring @N@ times is split into @N@ distinct names via the
@[v = v₁, …, v_N]@ construct, translated as ONE 'FreeVar' (@b@) agent plus a
chain of 'Copy' (@c@) agents fanning the value out to the @N@ occurrences. The
whole @c@-chain is built in 'buildList' (the free-variable list, OUTSIDE the
body), rooted at the @b@ agent's @fvValue@; 'compileVar' merely wires each
occurrence to the next @c@-chain output. Keeping the @c@ agents out of the body
means a @δ@ agent duplicating the body cannot duplicate them, so all @δ@ agents
stay well-balanced inside the body of the abstraction (§5 "Duplication").
-}
type FreeVarCtx = [Map String [Edge]]

{- | The captured variables of a term: the free variables of the term that are
bound by an enclosing binder (in the environment). Sorted for determinism.
-}
distinctFreeVars :: Env -> Lam -> [String]
distinctFreeVars env lam =
  sort (Set.toList (fv lam `Set.intersection` boundNames env))
 where
  boundNames :: Env -> Set String
  boundNames = Set.fromList . map symbol

{- | Count the occurrences of each variable in a term, EXCLUDING occurrences
inside nested abstractions that capture the variable. This gives the number of
'b@ agents that the enclosing abstraction must create for each captured
variable (one per occurrence wired to this abstraction's @b@ agents).

The @excluded@ set accumulates the free variables of nested abstractions as we
descend: an occurrence of a variable inside a nested @λw. body@ is handled by
that nested abstraction (which creates its own @b@ agents), not by the
enclosing one.
-}
countDirect :: Set String -> Lam -> Map String Int
countDirect excluded (LVar v)
  | v `Set.member` excluded = Map.empty
  | otherwise = Map.singleton v 1
countDirect excluded (LApp f x) =
  Map.unionWith (+) (countDirect excluded f) (countDirect excluded x)
countDirect excluded (LLam w body) =
  let innerFvs = fv body
      newExcluded = excluded `Set.union` innerFvs
   in Map.delete w (countDirect newExcluded body)
countDirect _ LSelfRef = Map.singleton selfRefName 1
countDirect excluded (LRec l) = Map.delete selfRefName (countDirect excluded l)

-- | Find the binder for a variable in the environment.
findBinder :: Env -> String -> Name
findBinder env v = case findBinderMaybe env v of
  Just n -> n
  Nothing -> error $ "findBinder: unbound variable " <> toText v

{- | Find the binder for a variable in the environment, returning 'Nothing'
if not found.
-}
findBinderMaybe :: Env -> String -> Maybe Name
findBinderMaybe env v = case env of
  [] -> Nothing
  n : ns
    | v == symbol n -> Just n
    | otherwise -> findBinderMaybe ns v

{- | Build the free-variable list: a chain of 'FreeVar' (@b@) agents, ONE per
captured variable (Mackie 1998, §4 "Abstraction": "one for each free
variable"), ending in an 'EndList' (@ν@) agent. Each @b@ agent's @fvValue@ is
wired to the enclosing binder edge (via 'reference') and is the ROOT of the
@N@-way @c@-chain (built here, in the free-variable list) that fans the value
out to the @N@ occurrences (the paper's @[v = v₁, …, v_N]@ translation). Its
@fvOcc@ is a fresh free edge. Returns the context mapping each variable to the
list of its @c@-chain output edges (one per occurrence, in body order).
-}
buildList :: Env -> Edge -> Map String Int -> BuildM (Map String [Edge])
buildList env eHead counts = go eHead (Map.keys counts) Map.empty
 where
  go :: Edge -> [String] -> Map String [Edge] -> BuildM (Map String [Edge])
  go eTail [] ctx = do
    void $ newNode EndList{endIn = eTail}
    return ctx
  go eCur (fvName : rest) ctx = do
    let n = Map.findWithDefault 0 fvName counts
    eOcc <- newEdge
    eCont <- newEdge
    refEdge <- reference (findBinder env fvName)
    void $ newNode FreeVar{fvPrimary = eCur, fvOcc = eOcc, fvCont = eCont, fvValue = refEdge, name = fvName}
    -- Fan the value out to @n@ occurrence edges. For @n == 1@ the value edge
    -- IS the occurrence edge (no @c@ agent). For @n >= 2@ a @c@-chain splits
    -- the value; it is rooted at the @b@ agent's @fvOcc@ (NOT @refEdge@, the
    -- enclosing binder edge). Rooting at @refEdge@ lets a @δ@ spawned on that
    -- binder edge (when the enclosing @λ@ is duplicated) meet the first
    -- @Copy@ and walk down the chain into the body, violating the paper's
    -- invariant that "@δ@ agents are well-balanced inside the body of an
    -- abstraction". Rooting at @fvOcc@ isolates the chain from the binder
    -- edge: the value reaches @fvOcc@ via the d×b rule (which merges
    -- @fvValue@ onto @fvOcc@ as the erasure agent walks the list), and the
    -- @c@ agents then fan it out to the @n@ occurrences.
    finalOccs <-
      if n == 1
        then return [refEdge]
        else do
          occs <- replicateM n newEdge
          buildCopyChain eOcc occs
          return occs
    go eCont rest (Map.insert fvName finalOccs ctx)

{- | Look up a variable in the free-variable context (from innermost to
outermost). Returns the updated context (with one @c@-chain output consumed
from the innermost map that still has one) and the output edge to wire the
occurrence to.
-}
lookupFvQueue :: FreeVarCtx -> String -> Maybe (FreeVarCtx, Edge)
lookupFvQueue ctx v = case ctx of
  [] -> Nothing
  m : ms -> case Map.lookup v m of
    Just (e : rest) -> Just (Map.insert v rest m : ms, e)
    _ -> lookupFvQueue ms v

-- * Net builder

{- | A net fragment: a function from an input edge to an output edge, built in
the @BuildM@ monad.
-}
type Projection = Edge -> BuildM Edge

-- | The identity projection.
idProj :: Projection
idProj = pure

{- | Build the YALE interaction net for a 'Lam' term. The net has a single
'Initiator' node whose @out@ port is the term's output edge. The compile-time
'Multiplexer' binders are compiled away by 'compileBinder'.
-}
buildYaleNet :: Lam -> Graph YaleNode
buildYaleNet = buildYaleNetK idProj

{- | Build the YALE interaction net for a 'Lam' term, then apply a
continuation (@proj@) to the term's output edge. The 'Initiator' node is the
root.
-}
buildYaleNetK :: Projection -> Lam -> Graph YaleNode
buildYaleNetK proj lam = execGraph build emptyGraph
 where
  build :: BuildM ()
  build = do
    eOut <- newEdge
    void $ newNode Initiator{out = eOut}
    void $ compile [] [] eOut lam
    -- Introduce the forcing agent (4) at the free output edge to drive
    -- strong reduction (Mackie §5.1): it supplies dummy arguments so that
    -- blocked computations complete, and toggles FreeVar <-> FreeVarBlocked
    -- as it walks the net.
    void $ newNode Forcing{forcingIn = eOut}
    void $ proj eOut

{- | Compile a 'Lam' term to the edge @p@ under the binder environment @env@
and the free-variable context @ctx@.
-}
compile :: Env -> FreeVarCtx -> Edge -> Lam -> BuildM FreeVarCtx
compile env ctx p term = case term of
  LApp f x -> do
    ef <- newEdge
    ex <- newEdge
    -- Application @M N@: appFunc = M (function, principal), appArg = N
    -- (argument), appResult = result (p).
    void $ newNode YaleApp{appFunc = ef, appArg = ex, appResult = p}
    ctx1 <- compile env ctx ef f
    compile env ctx1 ex x
  LLam v body -> do
    b <- newEdge
    eList <- newEdge
    (ev, name) <- bindName v
    -- Free-variable list (Mackie 1998): one 'FreeVar' (b) agent per CAPTURED
    -- variable of the body (free in the body, bound by an enclosing
    -- abstraction in @env@). The recursive self-reference (@selfRefName@) is
    -- NOT listed here: it is a bound variable of an enclosing abstraction
    -- (the @LRec@ binder), so @compileVar@ wires it DIRECTLY onto that
    -- binder edge. The β-rule's splice of the knot edge onto the binder edge
    -- then preserves the self-reference naturally — no 'FreeVar' intermediary
    -- is needed (and one would be erroneously erased by the β-rule).
    --
    -- A free variable not bound by any enclosing abstraction is a base value
    -- and is handled as a 'Constant' at its usage site (by @compileVar@).
    let fvs = distinctFreeVars env body
        -- One 'FreeVar' (@b@) agent per OCCURRENCE of each captured variable
        -- (not per distinct variable): this restores the paper's "unique
        -- occurrence" invariant per @b@ agent, so that a @δ@ agent parking at
        -- an occurrence is the only @δ@ on that @b@ agent's @fvOcc@ edge.
        counts = Map.restrictKeys (countDirect Set.empty body) (Set.fromList fvs)
    ctx' <- buildList env eList counts
    -- Abstraction @λv. body@: lamResult = result (p, principal), lamVarList =
    -- the free-variable list, lamVar = bound variable (ev), lamBody = body.
    -- Each occurrence of @v@ in the body is a direct port on the binder edge
    -- @ev@ (wired by @compileVar@); there is no separate marker on @ev@.
    void $ newNode YaleLam{lamResult = p, lamVarList = eList, lamVar = ev, lamBody = b}
    compile (name : env) (ctx' : ctx) b body
  LVar v -> compileVar env ctx p v
  LSelfRef -> compileVar env ctx p selfRefName
  LRec (LLam _ body) ->
    -- YALE "tie the knot" (Mackie 1998, §6 "Recursion"): @Y(F)@ where
    -- @F = λself. body'@ and @body' = body[LSelfRef ↦ self]@. The paper codes
    -- recursion "without needing to introduce any additional agents" — the
    -- knot is a cyclic structure using TWO application agents ("using two
    -- interactions", Fig. p.125):
    --
    --        p ◄─ @1.res
    --             @1  = YaleApp(func=N1, arg=eK, res=p)
    --        eK ◄─ @2.res
    --             @2  = YaleApp(func=N2, arg=eK, res=eK)
    --             N1, N2 = two copies of 𝒯(λself. body')
    --
    -- @eK@ is the knot edge: the argument of BOTH applications and the result
    -- of @2. When @2 fires (β-rule with appArg == appResult == eK), the body
    -- (with self↦eK) flows back to eK, regenerating the knot. The β-rule
    -- preserves eK in this case (see 'betaRHS' in YaleReduce.hs).
    --
    -- The self-reference is a BOUND variable of @F@ (spliced onto @eK@ when
    -- an @ fires); it is a free variable of any nested abstraction, so it
    -- enters that abstraction's free-variable list (a 'FreeVar' whose
    -- @fvOcc@ sits on @F@'s binder edge).
    do
      eK <- newEdge
      eN <- newEdge
      let body' = replaceSelfRef selfRefName body
      ctx1 <- compile env ctx eN (LLam selfRefName body')
      eRes <- newEdge
      void $ newNode YaleApp{appResult = eRes, appFunc = eN, appArg = eK}
      void $
        newNode
          Copy
            { copyIn = eRes
            , copyOut1 = p
            , copyOut2 = eK
            , copySticky = [eK]
            , copySplitVars = []
            }
      return ctx1
  LRec _ -> error "LRec: expected a lambda"

{- | Resolve a variable occurrence @v@ at the edge @p@ under the binder
environment @env@ and the free-variable context @ctx@.

If @v@ is a captured variable (present in @ctx@), the occurrence is wired to
the next output of the variable's @c@-chain (built in 'buildList', in the
free-variable list). This is the direct net translation of the paper's
@[v = v₁, …, v_N]@ construct (Mackie 1998, §4 "Duplication"): one @b@ agent
plus a @c@-chain fanning the value out to the @N@ occurrences; the @c@ agents
live outside the body, so a @δ@ duplicating the body cannot reach them.
Otherwise, the innermost matching binder registers the occurrence on its binder
'Multiplexer' (via 'reference'); a reference that is not bound by any enclosing
binder becomes a 'Constant'.
-}
compileVar :: Env -> FreeVarCtx -> Edge -> String -> BuildM FreeVarCtx
compileVar env ctx p v = case lookupFvQueue ctx v of
  Just (ctx', eOcc) ->
    -- Captured variable: wire the occurrence to the next @c@-chain output.
    mergeEdges eOcc p >> return ctx'
  Nothing -> case env of
    [] -> newNode Constant{inp = p, name = v} >> return ctx
    n : ns ->
      if v == symbol n
        then do
          e <- reference n
          mergeEdges e p
          return ctx
        else compileVar ns ctx p v

-- * Binder compilation

{- | Build a 'Copy' (@c@) chain that fans the edge @v@ out to the occurrence
edges @occs@. For a single occurrence, @v@ is merged directly onto it; for two
or more, a chain of 'Copy' agents splits @v@ so that each occurrence edge
receives one copy. This is the paper-faithful (Mackie 1998) linear encoding:
duplication is an explicit @c@ agent emitted by the compiler, not implicit
sharing detected by the reducer.
-}
buildCopyChain :: Edge -> [Edge] -> BuildM ()
buildCopyChain v occs = case occs of
  [] -> return () -- unreachable: 0 occurrences handled by 'Garbage'
  [occ] -> mergeEdges v occ
  [occ1, occ2] -> void $ newNode Copy{copyIn = v, copyOut1 = occ1, copyOut2 = occ2, copySticky = [], copySplitVars = []}
  (occ1 : rest) -> do
    eMid <- newEdge
    void $ newNode Copy{copyIn = v, copyOut1 = occ1, copyOut2 = eMid, copySticky = [], copySplitVars = []}
    buildCopyChain eMid rest

{- | Compile a 'Multiplexer' binder away. The bound variable's occurrences are
registered on the 'Multiplexer' by @compileVar@ (one edge per occurrence).
This rule inspects the occurrence count and emits the explicit linear
structure on the binder edge:
  * 0 occurrences: a 'Garbage' (@e@) agent erases the (future) argument.
  * 1 occurrence: the binder edge is merged directly onto the single
    occurrence edge.
  * ≥2 occurrences: a 'Copy' (@c@) chain fans the binder edge out to one copy
    per occurrence.

The 'Multiplexer' is a compile-time-only artifact and is deleted. Applied
exhaustively until no 'Multiplexer' remains.
-}
compileBinder :: Rule YaleNode
compileBinder = do
  Multiplexer{out = v, ins = occs} <- node
  rewrite $ \hist -> do
    -- Delete the compile-time 'Multiplexer'; its ports (the binder edge @v@
    -- and the occurrence edges @occs@) are freed, then re-wired below.
    mapM_ deleteNode hist
    case occs of
      [] -> void $ newNode Garbage{garbageIn = v}
      _ -> buildCopyChain v occs

-- | Apply 'compileBinder' exhaustively, removing all compile-time binders.
compileBinders :: Graph YaleNode -> Graph YaleNode
compileBinders = execGraph (apply $ exhaustive compileBinder)
