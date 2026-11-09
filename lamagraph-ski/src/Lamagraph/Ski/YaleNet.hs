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
  two copies, propagating a 'Subst' (@s@) agent into the body of a copied
  abstraction and a 'Copy' along its free-variable list.
* 'Subst' — the @s@ (substitution) agent. Propagated inside the body of a
  copied abstraction; it walks the body and is erased at leaves, which is what
  lets a copied (possibly cyclic) body re-connect correctly.
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
  rewire,
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
    -}
    Copy
      { copyIn :: Edge
      , copyOut1 :: Edge
      , copyOut2 :: Edge
      }
  | -- | The @s@ (substitution) agent, propagated inside a copied body.
    Subst
      { substIn :: Edge
      , substOut :: Edge
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
    Copy{copyIn = a, copyOut1 = b, copyOut2 = c} -> [a, b, c]
    Subst{substIn = a, substOut = b} -> [a, b]
    Erase{eraseIn = a} -> [a]
    FreeVar{fvCont = a, fvOcc = b, name = _} -> [a, b]
    FreeVarBlocked{fvHead = a, fvCont = b, fvOcc = c, name = _} -> [a, b, c]
    EndList{endIn = a} -> [a]
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
    Subst{} -> n{substIn = a, substOut = b}
     where
      [a, b] = ports
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
    Constant{inp = i} -> i
    Multiplexer{out = o} -> o

-- * Build monad and binder environment

-- | The build monad: the graph-building 'Rewrite' monad.
type BuildM = Rewrite YaleNode

{- | A bound name: its symbol and a 'reference' that appends a fresh input
edge to the binder 'Multiplexer' (wiring one occurrence of the variable).
-}
data Name = Name
  { symbol :: String
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
  return (v, Name{symbol = sym, reference = ref})

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
    -- ν, lamVar = bound variable (ev), lamBody = body.
    void $ newNode YaleLam{lamResult = p, lamVarList = eList, lamVar = ev, lamBody = b}
    compile (name : env) b body
  LVar v -> compileVar env p v
  LSelfRef -> compileVar env p selfRefName
  LRec inner ->
    -- YALE "tie the knot" (Mackie 1998, Fig. p.9): @Y(F)@ where
    -- @F = λself. inner@. Two application nodes, each fed by its own copy of
    -- the net for @F@: the top @ applies @F@ to the knot edge @eK@, producing
    -- the output @p@; the bottom @ applies @F@ to @eK@, producing @eK@ itself
    -- (its result port loops back to the knot — the self-loop). The knot edge
    -- @eK@ is the cyclic fixed point.
    do
      eK <- newEdge
      eF1 <- newEdge
      eF2 <- newEdge
      let fTerm = LLam selfRefName inner
      compile env eF1 fTerm
      compile env eF2 fTerm
      -- Top @: func = N1 (eF1), arg = eK (the knot), result = p (the output).
      void $ newNode YaleApp{appFunc = eF1, appArg = eK, appResult = p}
      -- Bottom @: func = N2 (eF2), arg = eK, result = eK (the knot).
      void $ newNode YaleApp{appFunc = eF2, appArg = eK, appResult = eK}
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
      then mergeEdges p =<< reference n
      else compileVar ns p v

-- * Binder compilation

{- | Compile a 'Multiplexer' binder away: no references leave the binder edge
dangling, one reference is a plain rewire, and several references become a
left-associated chain of 'Copy' agents fanning the binder edge out to the
references. Applied exhaustively until no 'Multiplexer' remains.
-}
compileBinder :: Rule YaleNode
compileBinder = do
  Multiplexer{out = o, ins = is} <- node
  let rs = reverse is
  case rs of
    [] -> replace $ return ()
    [r] -> rewire [[o, r]]
    (r0 : rs') ->
      replace $
        let go _ [] = return ()
            go acc (r : rest) =
              case rest of
                -- Last reference: the binder edge @o@ is the 'Copy' source.
                [] -> byNode $ Copy{copyIn = o, copyOut1 = acc, copyOut2 = r}
                _ -> do
                  f <- byEdge
                  byNode $ Copy{copyIn = f, copyOut1 = acc, copyOut2 = r}
                  go f rest
         in go r0 rs'

-- | Apply 'compileBinder' exhaustively, removing all compile-time binders.
compileBinders :: Graph YaleNode -> Graph YaleNode
compileBinders = execGraph (apply $ exhaustive compileBinder)
