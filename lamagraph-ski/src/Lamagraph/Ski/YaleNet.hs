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
  two copies, propagating a 'Copy' along the body and the free-variable list
  of a copied abstraction.
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

import Data.Set qualified as Set
import Data.View
import GraphRewriting (
  Edge,
  Graph,
  Rewrite,
  Rule,
  apply,
  emptyGraph,
  execGraph,
  exhaustive,
  mergeEdges,
  modifyNode,
  newEdge,
  newNode,
  node,
  replace,
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
  | {- | The @s@ (substitution) agent, propagated inside a copied body.
    @substVar@ is the bound-variable edge of the abstraction being duplicated
    (the original); @substVarCopy@ is the bound-variable edge of the copy this
    @s@ agent belongs to. The @s@ agent propagates into applications and nested
    abstractions and is erased at a base value.
    -}
    Subst
      { substIn :: Edge
      , substOut :: Edge
      , substVar :: Edge
      , substVarCopy :: Edge
      }
  | {- | The @d@ (eraser) agent, introduced at the head of a free-variable
    list by the β-rule.
    -}
    Erase
      { eraseIn :: Edge
      }
  | {- | A @b@ (free-variable) agent; @fvCont@ is the list continuation
    (principal), @fvOcc@ the variable occurrence.
    -}
    FreeVar
      { fvCont :: Edge
      , fvOcc :: Edge
      , name :: String
      }
  | {- | A @b'@ (blocked free-variable) agent. @fvHead@ is the principal
    port (where a @c@ agent meets it), @fvCont@ the list continuation
    (where the propagated @c@ goes next), @fvOcc@ the variable occurrence.
    -}
    FreeVarBlocked
      { fvHead :: Edge
      , fvCont :: Edge
      , fvOcc :: Edge
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
    Subst{substIn = a, substOut = b, substVar = c, substVarCopy = d} -> [a, b, c, d]
    Erase{eraseIn = a} -> [a]
    FreeVar{fvCont = a, fvOcc = b, name = _} -> [a, b]
    FreeVarBlocked{fvHead = a, fvCont = b, fvOcc = c, name = _} -> [a, b, c]
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
    Subst{} -> n{substIn = a, substOut = b, substVar = c, substVarCopy = d}
     where
      [a, b, c, d] = ports
    Erase{} -> n{eraseIn = a}
     where
      [a] = ports
    FreeVar{} -> n{fvCont = a, fvOcc = b}
     where
      [a, b] = ports
    FreeVarBlocked{} -> n{fvHead = a, fvCont = b, fvOcc = c}
     where
      [a, b, c] = ports
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
    Subst{substIn = a} -> a
    Erase{eraseIn = a} -> a
    FreeVar{fvCont = a} -> a
    FreeVarBlocked{fvHead = a} -> a
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

{- | Look up the binder edge of a name in the environment (innermost binder
first). Used to wire a 'FreeVar' occurrence onto the enclosing abstraction's
binder edge.
-}
lookupBinder :: String -> Env -> Maybe Edge
lookupBinder sym env = listToMaybe [binderEdge n | n <- env, symbol n == sym]

{- | Build one 'FreeVar' (b) agent for a free variable @w@ of an abstraction
body: its @fvCont@ port joins the list edge @eList@ and its @fvOcc@ port is
merged onto the binder edge of the enclosing abstraction that binds @w@. The
caller guarantees @w@ is bound in @env@ (see 'lookupBinder').
-}
buildFvNode :: Env -> String -> Edge -> BuildM ()
buildFvNode env w eList = do
  let Just binderEdge = lookupBinder w env
  eOcc <- newEdge
  -- Create the node FIRST (its @fvOcc@ port is on the fresh, live @eOcc@),
  -- THEN merge @eOcc@ onto the enclosing binder edge (moving the port). The
  -- node's @fvOcc@ field still names @eOcc@ afterwards, so patch it to the
  -- survivor to avoid a stale edge reference.
  fvNode <- newNode FreeVar{fvCont = eList, fvOcc = eOcc, name = w}
  mergeEdges binderEdge eOcc
  modifyNode fvNode $ \n -> case n of
    FreeVar{fvCont = a, name = c} -> FreeVar{fvCont = a, fvOcc = binderEdge, name = c}
    _ -> n

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
    compile [] eOut lam
    -- Introduce the forcing agent (4) at the free output edge to drive
    -- strong reduction (Mackie §5.1): it supplies dummy arguments so that
    -- blocked computations complete, and toggles FreeVar <-> FreeVarBlocked
    -- as it walks the net.
    void $ newNode Forcing{forcingIn = eOut}
    void $ proj eOut

-- | Compile a 'Lam' term to the edge @p@ under the binder environment @env@.
compile :: Env -> Edge -> Lam -> BuildM ()
compile env p term = case term of
  LApp f x -> do
    ef <- newEdge
    ex <- newEdge
    -- Application @M N@: appFunc = M (function, principal), appArg = N
    -- (argument), appResult = result (p).
    void $ newNode YaleApp{appFunc = ef, appArg = ex, appResult = p}
    compile env ef f
    compile env ex x
  LLam v body -> do
    b <- newEdge
    eList <- newEdge
    (ev, name) <- bindName v
    -- Free-variable list (Mackie 1998): one 'FreeVar' (b) agent per free
    -- variable of the body that must be SHARED (not duplicated) when the
    -- abstraction is copied — currently only the recursive self-reference
    -- (@selfRefName@), whose value is the tie-the-knot loop. All 'FreeVar's
    -- share the single list edge @eList@, terminated by 'EndList' (ν). Each
    -- 'FreeVar''s @fvOcc@ is merged onto the binder edge of the abstraction
    -- that binds the variable. When the 'Copy' agent duplicates this
    -- abstraction it walks the list and duplicates each 'FreeVar' while
    -- routing @fvOcc@ ('copyFreeVarRHS') — this is what makes the recursive
    -- self-reference shared rather than duplicated, preserving the
    -- tie-the-knot cycle.
    --
    -- Ordinary captured variables (bound by an enclosing abstraction) are NOT
    -- listed here: their occurrences are wired DIRECTLY onto the enclosing
    -- binder edge by @compileVar@, and 'copyLamRHS'\/'copyAppRHS' split that
    -- direct port across the copies (via 'byEdgeOrSticky'), so no 'FreeVar'
    -- is needed to track them. Listing them would create a spurious 'FreeVar'
    -- that survives to normal form under duplication (the inner copy's fv
    -- list is not β-reduced), sticking the net. A free variable not bound by
    -- any enclosing abstraction is a base value and is handled as a
    -- 'Constant' at its usage site (by @compileVar@), not listed here.
    let freeVars =
          filter
            (\w -> w == selfRefName && isJust (lookupBinder w env))
            (Set.toList (fv body Set.\\ Set.singleton v))
    mapM_ (\w -> buildFvNode env w eList) freeVars
    void $ newNode EndList{endIn = eList}
    -- Abstraction @λv. body@: lamResult = result (p, principal), lamVarList =
    -- the free-variable list, lamVar = bound variable (ev), lamBody = body.
    -- Each occurrence of @v@ in the body is a direct port on the binder edge
    -- @ev@ (wired by @compileVar@); there is no separate marker on @ev@.
    void $ newNode YaleLam{lamResult = p, lamVarList = eList, lamVar = ev, lamBody = b}
    compile (name : env) b body
  LVar v -> compileVar env p v
  LSelfRef -> compileVar env p selfRefName
  LRec (LLam _ body) ->
    -- YALE "tie the knot" (Mackie 1998, §6 "Recursion", Fig. p.125): @Y(F)@
    -- where @F = λself. body'@ and @body' = body[LSelfRef ↦ self]@. The knot
    -- is a SINGLE shared edge @eK@ that is simultaneously the argument of the
    -- application and the second output of the copy agent:
    --
    --        p ◄─ c.out1
    --             c  = Copy(in=eRes, out1=p, out2=eK)
    --             @  = YaleApp(func=N, arg=eK, res=eRes)
    --             N  = 𝒯(λself. body')
    --   eK is a single shared edge: @.appArg = c.out2 (the loop).
    --
    -- The self-reference is a BOUND variable of @F@ (spliced onto @eK@ when
    -- the @ fires); it is a free variable of any nested abstraction, so it
    -- enters that abstraction's free-variable list (a 'FreeVar' whose
    -- @fvOcc@ sits on @F@'s binder edge). The 'Copy' agent shares @eK@ (via
    -- 'copySticky') rather than duplicating it — this is the paper's
    -- "boundary wires are shared, not copied" behaviour, and is what
    -- preserves the tie-the-knot cycle.
    do
      eK <- newEdge
      eN <- newEdge
      let body' = replaceSelfRef selfRefName body
      compile env eN (LLam selfRefName body')
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
  LRec _ -> error "LRec: expected a lambda"
  LMutRec groups body ->
    -- Mutual recursion: one 'Multiplexer' per function; each body is compiled
    -- to its own output (self-references wired to it, cross-references by
    -- name), and the surrounding body is compiled with all names in scope.
    do
      (es, names) <- mapAndUnzipM (bindName . fst) groups
      let env' = names ++ env
      zipWithM_ (\e (n, b) -> compile env' e (replaceSelfRef n b)) es groups
      compile env' p body

{- | Resolve a variable occurrence @v@ at the edge @p@ under the binder
environment @env@. The innermost matching binder is wired directly; a
reference that is not bound by any enclosing binder becomes a 'Constant'.
-}
compileVar :: Env -> Edge -> String -> BuildM ()
compileVar env p v = case env of
  [] -> void $ newNode Constant{inp = p, name = v}
  n : ns ->
    if v == symbol n
      then
        -- Paper-faithful (Mackie 1998): a bound-variable occurrence is the
        -- binder edge itself (a hyperedge), with no marker node in between.
        -- Merging the occurrence edge @p@ onto the binder edge makes the
        -- occurrence a direct endpoint of the binder edge, so the β-rule's
        -- splice of the argument onto the binder edge is seen by every
        -- occurrence at once. The binder edge must be the SURVIVOR (first
        -- argument): 'mergeEdges' deletes its second argument, so passing the
        -- binder edge second would kill it after the first occurrence and
        -- crash on the second ('readEdge: edge ... does not exist').
        mergeEdges (binderEdge n) p
      else compileVar ns p v

-- * Binder compilation

{- | Compile a 'Multiplexer' binder away. Each bound-variable occurrence is
already wired DIRECTLY onto the binder edge by @compileVar@ (a direct port on
the binder hyperedge, no marker node in between), so the occurrences are
already connected to the binder edge. The 'Multiplexer' is a compile-time-only
artifact and is simply deleted. Applied exhaustively until no 'Multiplexer'
remains.
-}
compileBinder :: Rule YaleNode
compileBinder = do
  Multiplexer{out = _o, ins = _is} <- node
  replace $ return ()

-- | Apply 'compileBinder' exhaustively, removing all compile-time binders.
compileBinders :: Graph YaleNode -> Graph YaleNode
compileBinders = execGraph (apply $ exhaustive compileBinder)
