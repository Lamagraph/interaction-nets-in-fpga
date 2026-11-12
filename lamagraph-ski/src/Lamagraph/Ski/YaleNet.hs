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
  Projection,
  idProj,
  compileBinder,
  compileBinders,
) where

import Relude

import Data.View
import GraphRewriting (
  Edge,
  Graph,
  Rewrite,
  Rule,
  apply,
  byEdge,
  byNode,
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

import Lamagraph.Ski.Lambda (Lam (..), replaceSelfRef, selfRefName)

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
    @s@ agent belongs to. When the @s@ agent meets a 'BoundVar' marker whose
    port equals @substVar@, it rewires the occurrence to @substVarCopy@ (so the
    copy gets its own bound variable); otherwise it passes through (free var or
    a nested abstraction's bound variable).
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
  | {- | The @v@ (bound-variable) agent (Mackie 1998, Fig. 3a). A 2-port node
    that connects a bound-variable occurrence in the body to the binder edge
    of the abstraction.

    * @bvOcc@ is the occurrence port, sitting on the body edge where the
      variable is used. This is the principal port: it is the edge the @s@
      (substitution) agent meets when walking the copied body.
    * @bvEdge@ is the binder-edge port, sitting on the abstraction's
      bound-variable edge (@lamVar@). When the β-rule splices an argument onto
      the binder edge, this port carries the splice, so the occurrence is
      updated automatically.
    * @bvId@ is a STABLE identity for the bound variable: the binder edge at
      build time. The @s@ agent matches a marker by @bvId@ (not by @bvEdge@,
      which moves when the β-rule splices an argument onto the binder edge).

    Making the occurrence port the principal (rather than placing a single
    marker on the shared binder edge) is what keeps the dynamics race-free:
    the @c@ (copy) agent sits on the binder edge and therefore never meets the
    marker (whose principal is on the body edge), so it cannot accidentally
    consume the marker and sever the occurrence from the binder edge.
    -}
    BoundVar
      { bvOcc :: Edge
      , bvEdge :: Edge
      , bvId :: Edge
      }
  | -- | The @e@ (garbage) agent; erases whatever it meets.
    Garbage
      { garbageIn :: Edge
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
    BoundVar{bvOcc = a, bvEdge = b, bvId = _} -> [a, b]
    Garbage{garbageIn = a} -> [a]
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
    BoundVar{} -> n{bvOcc = a, bvEdge = b}
     where
      [a, b] = ports
    Garbage{} -> n{garbageIn = a}
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
    BoundVar{bvOcc = a} -> a
    Garbage{garbageIn = a} -> a
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

-- | The binder environment: a list of 'Name's, innermost binder first.
type Env = [Name]

{- | Bind a name, creating its binder 'Multiplexer'. Returns the binder edge
and the 'Name' (whose 'reference' wires variable occurrences).
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

-- * Net builder

{- | A net fragment: a function from an input edge to an output edge, built in
the 'BuildM' monad.
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
    -- The free-variable list of a freshly-built abstraction is empty: an
    -- 'EndList' (ν) agent. (Free variables are tracked only for open terms;
    -- all terms compiled here are closed, so the list is always just ν.)
    void $ newNode EndList{endIn = eList}
    -- Abstraction @λv. body@: lamResult = result (p, principal), lamVarList =
    -- ν, lamVar = bound variable (ev), lamBody = body. Each occurrence of @v@
    -- in the body carries its own 'BoundVar' marker (created by
    -- 'compileVar'), whose @bvEdge@ port sits on the binder edge @ev@; there
    -- is no separate marker on @ev@ itself.
    void $ newNode YaleLam{lamResult = p, lamVarList = eList, lamVar = ev, lamBody = b}
    compile (name : env) b body
  LVar v -> compileVar env p v
  LSelfRef -> compileVar env p selfRefName
  LRec (LLam v body) ->
    -- YALE "tie the knot" (Mackie 1998, §6 "Recursion"): @Y(F)@ where
    -- @F = λv. body@. Two 'YaleApp' nodes, each fed by its own copy of the
    -- net for @F@: the top app applies @F@ to the knot edge, producing the
    -- output; the bottom app applies @F@ to the knot edge, producing the
    -- knot edge itself (the self-loop). When the β-rule fires, the knot edge
    -- is spliced onto the bound variable, so the body's use of @v@ becomes
    -- the knot edge — the recursive self-reference.
    do
      let body' = replaceSelfRef v body
      eF1 <- newEdge
      compile env eF1 (LLam v body')
      eF2 <- newEdge
      compile env eF2 (LLam v body')
      eK <- newEdge
      void $ newNode YaleApp{appResult = p, appFunc = eF1, appArg = eK}
      void $ newNode YaleApp{appResult = eK, appFunc = eF2, appArg = eK}
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

{- | Compile a 'Multiplexer' binder away. Each occurrence already carries its
own 'BoundVar' marker (created by 'compileVar'), whose @bvOcc@ port sits on the
occurrence edge and whose @bvEdge@ port sits on the binder edge; the markers
therefore already connect the occurrences to the binder edge. The 'Multiplexer'
is a compile-time-only artifact and is simply deleted. Applied exhaustively
until no 'Multiplexer' remains.
-}
compileBinder :: Rule YaleNode
compileBinder = do
  Multiplexer{out = _o, ins = _is} <- node
  replace $ return ()

-- | Apply 'compileBinder' exhaustively, removing all compile-time binders.
compileBinders :: Graph YaleNode -> Graph YaleNode
compileBinders = execGraph (apply $ exhaustive compileBinder)
