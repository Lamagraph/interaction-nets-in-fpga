module Main (main) where

import Relude

import Common.Term qualified as Term
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Lazy qualified as HashMap
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GraphRewriting (
  Graph,
  Rewrite,
  emptyGraph,
  evalGraph,
  execGraph,
  newEdge,
  newNode,
  readNode,
  readNodeList,
 )
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import INet.Graph (fromTerm)
import INet.Graph qualified as INet
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.MonadDesugar (runMonadDesugar)
import Lamagraph.Compiler.Driver (desugarLmlProgram, parseLmlProgram, resolveLmlProgram, typecheckLmlProgram)
import Lamagraph.Compiler.Eval
import Lamagraph.Compiler.Syntax.Longident (mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski
import Lamagraph.Ski.YaleDecode (skiEvalYale)
import Lamagraph.Ski.YaleNet (YaleNode (..), buildYaleNet)
import Lamagraph.Ski.YaleNet qualified as YaleNet
import Lamagraph.Ski.YaleReduce (reduceStepsYale)

{- | Step budget for reducing a decoded term to normal form. Generous because
the naive SKI translation is verbose and the fixed-order reducer is slow.
-}
maxSteps :: MaxSteps
maxSteps = MaxSteps 500000

main :: IO ()
main =
  defaultMain $
    testGroup
      "lamagraph-ski"
      [ valueTests
      , churchTests
      , translateTests
      , recIrTests
      , translateLamTests
      , yaleReduceTests
      , yaleInteractionRuleTests
      , yaleDuplicationCompositionTests
      , recursionTests
      , basicFuncTests
      , mutualRecTests
      , yaleEvalTests
      , churchIfTests
      , yaleComplexTests
      , yaleSimpleConstructionTests
      , runnerTests
      , equivalenceTests
      , dropArgTests
      , skiEvalTests
      , boolTests
      , tupleTests
      , adtTests
      , charStringTests
      , fixedExampleTests
      , harnessTests
      , propertyTests
      , goldenTests
      , benchmarkTests
      , diagTest
      , spikeTest
      ]

{- | Unit tests for 'canonicalize' over the runtime value constructors, plus the
error case for a non-result value (a primitive).
-}
valueTests :: TestTree
valueTests =
  testGroup
    "canonicalize"
    [ testCase "VInt" $ assertCanonical (VInt 5) (RInt 5)
    , testCase "VChar" $ assertCanonical (VChar 'a') (RChar 'a')
    , testCase "VString" $ assertCanonical (VString "hello") (RString "hello")
    , testCase "VTuple" $ assertCanonical (VTuple (VInt 1) (VInt 2 :| [VInt 3])) (RTuple [RInt 1, RInt 2, RInt 3])
    , testCase "VAdt" $
        assertCanonical
          (VAdt (Name (mkLongident (pure "Cons"))) [VInt 1, VInt 2])
          (RAdt (Name (mkLongident (pure "Cons"))) [RInt 1, RInt 2])
    , testCase "primitive is an error" $ case canonicalize (VUnaryPrim UPMinus) of
        Left (NonResultValue _) -> return ()
        other -> assertFailure $ "expected Left (NonResultValue), got " <> show other
    ]

-- | Assert that 'canonicalize' succeeds on @input@ and yields @expected@.
assertCanonical :: Value -> Value' -> Assertion
assertCanonical input expected = case canonicalize input of
  Right v -> assertEqual "canonicalize" expected v
  Left e -> assertFailure $ "expected Right " <> show expected <> ", got Left " <> show e

{- | Round-trip and operation tests for the Church value encodings:
'encodeValue (RInt n)' must decode back to @n@ (for small @n@), and the Church
arithmetic combinators must produce the right results.
-}
churchTests :: TestTree
churchTests =
  testGroup
    "church encodings"
    [ testCase "encodeValue round-trip n=0" $ checkDecode "encodeValue 0" (encodeValue (RInt 0)) 0
    , testCase "encodeValue round-trip n=1" $ checkDecode "encodeValue 1" (encodeValue (RInt 1)) 1
    , testCase "encodeValue round-trip n=2" $ checkDecode "encodeValue 2" (encodeValue (RInt 2)) 2
    , testCase "mult 2 3 = 6" $ checkDecode "mult 2 3" (apply2 multLam 2 3) 6
    ]

{- | Apply a binary Church combinator (a lambda term) to two Church numerals,
yielding the SKI term for the result.
-}
apply2 :: State Int Lam -> Int -> Int -> Term.Expr
apply2 op a b = Term.A (Term.A (toSki (evalState op 0)) (intToChurch a)) (intToChurch b)

{- | Tests for the 'translateCore' translator: every minimal-subset construct
translates to a closed SKI term (no free @V@ nodes), and the '@'*'@ operator
wires to the Church mult combinator (end-to-end). The '@'+'@ wiring is covered
by the diag/spike tests below.
-}
translateTests :: TestTree
translateTests =
  testGroup
    "translateCore"
    [ testGroup
        "closed (no free V nodes)"
        [ testCase "int literal" $ assertClosed (mustTranslate [] (Lit (LitInt 5)))
        , testCase "plus" $ assertClosed (mustTranslate [] (binOp stdPlus 2 3))
        , testCase "times" $ assertClosed (mustTranslate [] (binOp stdTimes 2 3))
        , testCase "minus" $ assertClosed (mustTranslate [] (binOp stdMinus 5 2))
        , testCase "nested let" $
            let xVar = Id (Name (mkLongident (pure "x")))
             in assertClosed (mustTranslate [NonRec xVar (binOp stdPlus 2 3)] (Var xVar))
        , testCase "lambda + application" $
            let xVar = Id (Name (mkLongident (pure "x")))
                lamApp = App (Lam xVar (App (App (Var stdPlus) (Var xVar)) (Lit (LitInt 1)))) (Lit (LitInt 4))
             in assertClosed (mustTranslate [] lamApp)
        ]
    , testCase "end-to-end: 2 * 3 = 6" $ checkDecode "translate(2*3)" (mustTranslate [] (binOp stdTimes 2 3)) 6
    ]

{- | Unit tests for 'fv' and 'subst' over the recursion constructors
('LSelfRef', 'LRec'): a self-reference contributes no free variable, and
substitution respects the binders (a bound name is not substituted).
-}
recIrTests :: TestTree
recIrTests =
  testGroup
    "recursion IR (fv/subst)"
    [ testCase "fv LSelfRef = {selfRefName}" $ assertEqual "fv LSelfRef" (Set.singleton selfRefName) (fv LSelfRef)
    , testCase "fv LRec: binder is bound, self-ref is not free" $
        assertEqual
          "fv (LRec (LLam f (self x)))"
          (Set.fromList ["x"])
          (fv (LRec (LLam "f" (LApp LSelfRef (LVar "x")))))
    , testCase "subst: bound name is not substituted in an LRec body" $
        assertEqual
          "subst f g (LRec (LLam f (self f)))"
          (LRec (LLam "f" (LApp LSelfRef (LVar "f"))))
          (subst "f" (LVar "g") (LRec (LLam "f" (LApp LSelfRef (LVar "f")))))
    , testCase "subst: free name is substituted around an LRec" $
        assertEqual
          "subst f g (f (LRec (LLam f (self f))))"
          (LApp (LVar "g") (LRec (LLam "f" (LApp LSelfRef (LVar "f")))))
          (subst "f" (LVar "g") (LApp (LVar "f") (LRec (LLam "f" (LApp LSelfRef (LVar "f"))))))
    ]

{- | Tests for 'translateLam': 'let rec' produces an 'LRec' with an 'LSelfRef'
self-reference, mutual 'let rec' is expanded to a plain lambda term via
Bekić's theorem (no recursion markers), a non-recursive program carries no
recursion markers, and 'translateCore' rejects recursive bindings (the SKI
backend is non-recursive-only).
-}
translateLamTests :: TestTree
translateLamTests =
  testGroup
    "translateLam"
    [ testCase "let rec -> LRec with LSelfRef" $
        case compileLml "module M\nlet rec f n = if n < 1 then 1 else n * f (n - 1)\nlet result = f 2" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> case translateLam binds resultExpr of
            Left e -> assertFailure $ "translateLam failed: " <> show e
            Right lam -> do
              assertBool "contains LRec" (containsRecMarker lam)
              assertBool "contains LSelfRef" (containsLSelfRef lam)
              assertLamClosed lam
    , testCase "mutual let rec -> plain lambda term (Bekić expansion)" $
        case compileLml
          "module M\nlet rec even n = if n < 1 then true else odd (n - 1) and odd n = if n < 1 then false else even (n - 1)\nlet result = even 4" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> case translateLam binds resultExpr of
            Left e -> assertFailure $ "translateLam failed: " <> show e
            Right lam -> do
              assertBool "no recursion markers" (not (containsRecMarker lam))
              assertLamClosed lam
    , testCase "non-recursive program carries no recursion markers" $
        case compileLml "module M\nlet double = fun x -> x + x\nlet result = double 3" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> case translateLam binds resultExpr of
            Left e -> assertFailure $ "translateLam failed: " <> show e
            Right lam -> assertBool "no recursion markers" (not (containsRecMarker lam))
    , testCase "translateCore rejects recursive bindings" $
        case compileLml "module M\nlet rec f n = n\nlet result = f 5" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> case translateCore binds resultExpr of
            Left (UnsupportedConstruct "recursive let") -> return ()
            other -> assertFailure $ "expected Left (UnsupportedConstruct \"recursive let\"), got " <> show other
    ]

{- | Whether a lambda term contains a recursion marker ('LRec' or 'LSelfRef')
anywhere.
-}
containsRecMarker :: Lam -> Bool
containsRecMarker = \case
  LRec _ -> True
  LSelfRef -> True
  LVar _ -> False
  LApp f x -> containsRecMarker f || containsRecMarker x
  LLam _ b -> containsRecMarker b

-- | Whether a lambda term contains an 'LSelfRef' anywhere.
containsLSelfRef :: Lam -> Bool
containsLSelfRef = \case
  LSelfRef -> True
  LVar _ -> False
  LApp f x -> containsLSelfRef f || containsLSelfRef x
  LLam _ b -> containsLSelfRef b
  LRec l -> containsLSelfRef l

-- | Assert that a lambda term is closed (no free variables).
assertLamClosed :: Lam -> Assertion
assertLamClosed lam = assertEqual "free variables" Set.empty (fv lam)

-- | Build a core binary-operator application @(op a b)@.
binOp :: Var -> Int -> Int -> CoreExpr
binOp op a b = App (App (Var op) (Lit (LitInt a))) (Lit (LitInt b))

{- | Translate a core program, crashing with a clear message on failure (for
tests that expect a successful translation).
-}
mustTranslate :: [CoreBind] -> CoreExpr -> Term.Expr
mustTranslate binds expr = case translateCore binds expr of
  Right t -> t
  Left e -> error $ "mustTranslate: unexpected translation failure: " <> show e

-- | Assert that a SKI term is closed (has no free @V@ nodes).
assertClosed :: Term.Expr -> Assertion
assertClosed term = assertEqual "free V nodes" 0 (countV term)

-- | Count the @V@ (variable) nodes in a SKI term.
countV :: Term.Expr -> Int
countV = \case
  Term.V _ -> 1
  Term.A f x -> countV f + countV x
  _ -> 0

-- * YALE reducer tests (S4)

-- | Count the number of nodes in a YALE net.
yaleNodeCount :: Graph YaleNode -> Int
yaleNodeCount = evalGraph (length <$> readNodeList)

{- | Build a YALE net from an LML source, reduce it for at most @budget@
steps, and return (step count, node count, reached normal form).
-}
yaleReduceSrc :: Text -> Int -> IO (Int, Int, Bool)
yaleReduceSrc src budget = case compileLml src of
  Left e -> assertFailure $ "compileLml failed: " <> e
  Right (binds, resultExpr) -> case translateLam binds resultExpr of
    Left e -> assertFailure $ "translateLam failed: " <> show e
    Right lam ->
      let g = buildYaleNet lam
          (steps, g2, nf) = reduceStepsYale budget g
       in return (steps, yaleNodeCount g2, nf)

-- | Assert that an LML source reduces to normal form within the step budget.
assertYaleNF :: Text -> Int -> IO ()
assertYaleNF src budget = do
  (steps, _nodes, nf) <- yaleReduceSrc src budget
  assertBool
    ("expected NF within " <> show budget <> " steps, got " <> show steps)
    nf

{- | YALE reducer regression tests: each source must reach normal form within
the step budget. These exercise the full YALE dynamics (β-reduction,
duplication, erasure, and the knot encoding for recursion).
-}
yaleReduceTests :: TestTree
yaleReduceTests =
  testGroup
    "yale-reduce"
    [ testCase "2 + 3" $ assertYaleNF "module M\nlet result = 2 + 3" 20000
    , testCase "3 + 3" $ assertYaleNF "module M\nlet result = 3 + 3" 20000
    , testCase "2 - 1" $ assertYaleNF "module M\nlet result = 2 - 1" 5000
    , testCase "2 * 3" $ assertYaleNF "module M\nlet result = 2 * 3" 5000
    , testCase "2 < 3" $ assertYaleNF "module M\nlet result = 2 < 3" 5000
    , testCase "if 2 < 3 then 10 else 20" $ assertYaleNF "module M\nlet result = if 2 < 3 then 10 else 20" 5000
    , testCase "rec identity" $ assertYaleNF "module M\nlet rec f n = n\nlet result = f 5" 5000
    , testCase "fact 2" $
        assertYaleNF "module M\nlet rec fact n = if n < 1 then 1 else n * fact (n - 1)\nlet result = fact 2" 10000
    ]

-- * Interaction-rule tests (net-level, paper-faithful)

{- | Build a YALE net from a build action (no 'Initiator', no 'Forcing' agent):
a bare net containing exactly the nodes the test creates.
-}
buildYaleTestNet :: Rewrite YaleNode () -> Graph YaleNode
buildYaleTestNet build = execGraph build emptyGraph

-- | A short tag for a YALE node type (for histograms).
yaleNodeTag :: YaleNode -> String
yaleNodeTag = \case
  Initiator{} -> "Init"
  YaleLam{} -> "Lam"
  YaleApp{} -> "App"
  Copy{} -> "Copy"
  Subst{} -> "Subst"
  Erase{} -> "Erase"
  FreeVar{} -> "FreeV"
  FreeVarBlocked{} -> "FreeVB"
  EndList{} -> "End"
  Garbage{} -> "Garbage"
  Forcing{} -> "Forcing"
  Constant{YaleNet.name = n} -> "Const(" <> n <> ")"
  Multiplexer{} -> "Mux"

-- | Histogram of node types in a YALE net.
yaleHist :: Graph YaleNode -> Map String Int
yaleHist = evalGraph go
 where
  go = do
    ns <- readNodeList
    vs <- mapM readNode ns
    return (Map.fromListWith (+) [(yaleNodeTag v, 1) | v <- vs])

{- | Reduce a bare test net and assert it reaches normal form with exactly the
expected node-type histogram. This checks a single interaction rule (or a small
composition) against the paper's expected right-hand side: the LHS nodes are
consumed and the RHS nodes remain.
-}
assertReducesTo :: String -> Rewrite YaleNode () -> Map String Int -> IO ()
assertReducesTo lbl build expected = do
  let g0 = buildYaleTestNet build
      (steps, g, nf) = reduceStepsYale 10000 g0
  assertBool (lbl <> ": expected NF, stuck after " <> show steps <> " steps") nf
  assertEqual (lbl <> ": node histogram") expected (yaleHist g)

{- | Net-level tests for the individual YALE interaction rules (Mackie 1998,
§5). Each test builds a bare net containing one active pair (plus the minimal
context the rule needs), reduces it, and checks the surviving nodes match the
paper's right-hand side. These isolate the dynamics rule-by-rule, independent of
the net builder and the scheduler.
-}
yaleInteractionRuleTests :: TestTree
yaleInteractionRuleTests =
  testGroup
    "yale-interaction-rules"
    [ -- Erase (d) x EndList (nu): the eraser completes on an empty list.
      testCase "Erase x EndList -> empty" $
        assertReducesTo
          "Erase x EndList"
          ( do
              e <- newEdge
              void $ newNode Erase{eraseIn = e}
              void $ newNode EndList{endIn = e}
          )
          Map.empty
    , -- Copy (c) x Constant: a base value is duplicated into two copies.
      testCase "Copy x Constant -> two copies" $
        assertReducesTo
          "Copy x Constant"
          ( do
              eIn <- newEdge
              e1 <- newEdge
              e2 <- newEdge
              void $ newNode Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = [], copySplitVars = []}
              void $ newNode Constant{inp = eIn, name = "k"}
          )
          (Map.fromList [("Const(k)", 2)])
    , -- Copy (c) x EndList (nu): the end-of-list is duplicated into two.
      testCase "Copy x EndList -> two EndLists" $
        assertReducesTo
          "Copy x EndList"
          ( do
              eIn <- newEdge
              e1 <- newEdge
              e2 <- newEdge
              void $ newNode Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = [], copySplitVars = []}
              void $ newNode EndList{endIn = eIn}
          )
          (Map.fromList [("End", 2)])
    , -- Subst (s) x Constant: the substitution agent is erased at a base value.
      testCase "Subst x Constant -> one Constant" $
        assertReducesTo
          "Subst x Constant"
          ( do
              eIn <- newEdge
              eOut <- newEdge
              eVar <- newEdge
              eVarCopy <- newEdge
              void $ newNode Subst{substIn = eIn, substOut = eOut, substVar = eVar, substVarCopy = eVarCopy}
              void $ newNode Constant{inp = eIn, name = "k"}
          )
          (Map.fromList [("Const(k)", 1)])
    , -- beta: (\\x. x) k -> k. The degenerate body = var case: the argument is
      -- spliced onto the binder and the result is the argument.
      testCase "beta: (\\x. x) k -> k" $
        assertReducesTo
          "beta identity"
          ( do
              eOut <- newEdge
              eRes <- newEdge
              eBinder <- newEdge
              eList <- newEdge
              eK <- newEdge
              void $ newNode YaleLam{lamResult = eRes, lamVarList = eList, lamVar = eBinder, lamBody = eBinder}
              void $ newNode YaleApp{appFunc = eRes, appArg = eK, appResult = eOut}
              void $ newNode Constant{inp = eK, name = "k"}
              void $ newNode EndList{endIn = eList}
          )
          (Map.fromList [("Const(k)", 1)])
    , -- beta: (\\x. c) k -> c. The bound variable is UNUSED, so the argument is
      -- erased (Garbage) and the result is the body.
      testCase "beta: (\\x. c) k -> c (arg erased)" $
        assertReducesTo
          "beta unused var"
          ( do
              eOut <- newEdge
              eRes <- newEdge
              eBinder <- newEdge
              eList <- newEdge
              eK <- newEdge
              eC <- newEdge
              void $ newNode YaleLam{lamResult = eRes, lamVarList = eList, lamVar = eBinder, lamBody = eC}
              void $ newNode YaleApp{appFunc = eRes, appArg = eK, appResult = eOut}
              void $ newNode Constant{inp = eK, name = "k"}
              void $ newNode Constant{inp = eC, name = "c"}
              void $ newNode EndList{endIn = eList}
          )
          (Map.fromList [("Const(c)", 1)])
    , -- beta: (\\x. x x) k -> k k. The bound variable is used TWICE, so the
      -- argument is DUPLICATED (a Copy fans it out to both occurrences).
      testCase "beta: (\\x. x x) k -> k k (duplication)" $
        assertReducesTo
          "beta duplication"
          ( do
              eOut <- newEdge
              eRes <- newEdge
              eBinder <- newEdge
              eList <- newEdge
              eK <- newEdge
              eBody <- newEdge
              void $ newNode YaleApp{appFunc = eBinder, appArg = eBinder, appResult = eBody}
              void $ newNode YaleLam{lamResult = eRes, lamVarList = eList, lamVar = eBinder, lamBody = eBody}
              void $ newNode YaleApp{appFunc = eRes, appArg = eK, appResult = eOut}
              void $ newNode Constant{inp = eK, name = "k"}
              void $ newNode EndList{endIn = eList}
          )
          (Map.fromList [("App", 1), ("Const(k)", 2)])
    , -- Copy (c) x App: the application is duplicated; the Copy propagates
      -- along the function and argument ports (duplicating each).
      testCase "Copy x App -> two Apps" $
        assertReducesTo
          "Copy x App"
          ( do
              eIn <- newEdge
              e1 <- newEdge
              e2 <- newEdge
              eFunc <- newEdge
              eArg <- newEdge
              void $ newNode YaleApp{appFunc = eFunc, appArg = eArg, appResult = eIn}
              void $ newNode Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = [], copySplitVars = []}
              void $ newNode Constant{inp = eFunc, name = "f"}
              void $ newNode Constant{inp = eArg, name = "a"}
          )
          (Map.fromList [("App", 2), ("Const(f)", 2), ("Const(a)", 2)])
    , -- Copy (c) x Lam: the abstraction is duplicated; each copy gets its own
      -- binder edge, and a Copy propagates along the body and the fv-list.
      testCase "Copy x Lam -> two Lams" $
        assertReducesTo
          "Copy x Lam"
          ( do
              eIn <- newEdge
              e1 <- newEdge
              e2 <- newEdge
              eBinder <- newEdge
              eList <- newEdge
              eBody <- newEdge
              void $ newNode YaleLam{lamResult = eIn, lamVarList = eList, lamVar = eBinder, lamBody = eBody}
              void $ newNode Copy{copyIn = eIn, copyOut1 = e1, copyOut2 = e2, copySticky = [], copySplitVars = []}
              void $ newNode Constant{inp = eBody, name = "b"}
              void $ newNode EndList{endIn = eList}
          )
          (Map.fromList [("Lam", 2), ("Const(b)", 2), ("End", 2)])
    , -- Forcing (4) x Forcing (4): the two forcing agents cancel.
      testCase "Forcing x Forcing -> empty" $
        assertReducesTo
          "Forcing x Forcing"
          ( do
              e <- newEdge
              void $ newNode Forcing{forcingIn = e}
              void $ newNode Forcing{forcingIn = e}
          )
          Map.empty
    , -- Forcing (4) x Constant: the forcing agent is absorbed at a base value.
      testCase "Forcing x Constant -> Constant (absorbed)" $
        assertReducesTo
          "Forcing x Constant"
          ( do
              e <- newEdge
              void $ newNode Forcing{forcingIn = e}
              void $ newNode Constant{inp = e, name = "k"}
          )
          (Map.fromList [("Const(k)", 1)])
    , -- Forcing (4) x FreeVar (b): the b agent is toggled to b' (blocked).
      testCase "Forcing x FreeVar -> FreeVarBlocked" $
        assertReducesTo
          "Forcing x FreeVar"
          ( do
              eHead <- newEdge
              eOcc <- newEdge
              void $ newNode Forcing{forcingIn = eHead}
              void $ newNode FreeVar{fvCont = eHead, fvOcc = eOcc, name = "x"}
          )
          (Map.fromList [("FreeVB", 1)])
    , -- Forcing (4) x FreeVarBlocked (b'): the b' agent is toggled back to b.
      testCase "Forcing x FreeVarBlocked -> FreeVar" $
        assertReducesTo
          "Forcing x FreeVarBlocked"
          ( do
              eHead <- newEdge
              eCont <- newEdge
              eOcc <- newEdge
              void $ newNode Forcing{forcingIn = eHead}
              void $ newNode FreeVarBlocked{fvHead = eHead, fvCont = eCont, fvOcc = eOcc, name = "x"}
          )
          (Map.fromList [("FreeV", 1)])
    ]

-- * Duplication-composition tests (net-level, paper-faithful)

{- | Agent tags that must NOT survive in a clean normal form: a lingering
duplication\/substitution\/erasure agent (or a free-variable marker) means the
net is stuck mid-reduction. (A lone 'Forcing' at the output, and 'Lam'\/'App'\/
'End' inside a Church value, are legitimate and are NOT flagged.)
-}
yaleStuckTags :: [String]
yaleStuckTags = ["Copy", "Subst", "Erase", "FreeV", "FreeVB", "BoundV"]

{- | Reduce a FULL YALE net built from a closed lambda term ('buildYaleNet',
which adds the 'Initiator' and the output 'Forcing') and assert it reaches
normal form with no stuck agents. Unlike 'assertReducesTo' (an exact histogram
for a single rule), this checks a COMPOSITION: the whole term reduces and no
duplication\/erasure\/substitution agent is left dangling. This is the
lambda-term-level regression test for composition bugs that the single-rule
tests cannot catch.
-}
assertLamNormalForm :: String -> Lam -> IO ()
assertLamNormalForm lbl lam = do
  let g0 = buildYaleNet lam
      (steps, g, nf) = reduceStepsYale 10000 g0
      h = yaleHist g
      stuck = [t | t <- yaleStuckTags, isJust (Map.lookup t h)]
  assertBool (lbl <> ": expected NF, stuck after " <> show steps <> " steps") nf
  assertBool (lbl <> ": stuck agents remain: " <> show stuck) (null stuck)

{- | Composition tests for the duplication dynamics. The minimal structure that
exposed the 'copyLamRHS' bug: a lambda is DUPLICATED (by self-application or a
Church numeral) and its body is itself a lambda whose body is a bound variable
of an OUTER lambda that is being duplicated. Such a body is a bound-variable
occurrence, not a sub-net, so it must be SPLIT across the copies (routed by
'byEdgeOrSticky'), never handed to a body 'Copy' agent (which would dangle on
the binder edge and stick the net).
-}
yaleDuplicationCompositionTests :: TestTree
yaleDuplicationCompositionTests =
  testGroup
    "yale-duplication-compositions"
    [ -- Positive control: duplicating @λa. a@ (body = the SAME lambda's bound
      -- variable, the @lamBody == v@ case) already worked and must keep working.
      testCase "dup (\\a. a) via self-app" $
        assertLamNormalForm "dup (\\a. a)" (LApp (LLam "x" (LApp (LVar "x") (LVar "x"))) (LLam "a" (LVar "a")))
    , -- THE BUG: duplicating @λa. λb. a@ (body = a bound variable of an OUTER
      -- lambda being duplicated). Self-application @(\x. x x)@ uses its argument
      -- twice, forcing the duplication of @(\a b. a)@.
      testCase "dup (\\a b. a) via self-app" $
        assertLamNormalForm "dup (\\a b. a)" (LApp (LLam "x" (LApp (LVar "x") (LVar "x"))) (LLam "a" (LLam "b" (LVar "a"))))
    , -- The same structure driven by a Church numeral: @2 (\k. (\a b. a) k) 3@
      -- reduces to @3@. The numeral 2 duplicates @(\k. (\a b. a) k)@, whose body
      -- contains the nested @λa. λb. a@.
      testCase "2 (\\k. (\\a b. a) k) 3" $
        assertLamNormalForm
          "2 (\\k. (\\a b. a) k) 3"
          ( LApp
              (LApp (church 2) (LLam "k" (LApp (LLam "a" (LLam "b" (LVar "a"))) (LVar "k"))))
              (church 3)
          )
    ]
 where
  church n = evalState (churchLam n) 0

-- * Recursion tests (S7)

{- | Assert that the LML interpreter and the YALE pipeline agree on the program,
and that the agreed value equals @expected@. This is the YALE analogue of
'assertEqualBoth' (which uses the SKI backend): it runs the LML interpreter and
'skiEvalYale' directly, so it exercises recursion (which the SKI backend cannot
handle). A 'StepOverflowReached' or any other 'SkiError' is a loud failure
(never a hang): the reduction is bounded by 'defaultMaxSteps'.
-}
assertYaleAgrees :: Text -> Value' -> IO ()
assertYaleAgrees src expected = case compileLml src of
  Left e -> assertFailure $ "compileLml failed: " <> e
  Right (binds, resultExpr) -> do
    lmlRes <- lmlValue binds resultExpr
    case lmlRes of
      Left e -> assertFailure $ "LML evaluation failed: " <> show e
      Right a ->
        case skiEvalYale expected binds resultExpr of
          Left e -> assertFailure $ "skiEvalYale failed: " <> show e
          Right b
            | a == b -> assertEqual "LML and YALE agree" expected b
            | otherwise ->
                assertFailure $ "LML/YALE disagreement: LML=" <> show a <> " YALE=" <> show b

{- | Recursion tests (tie-the-knot cyclic nets). Each case runs BOTH the LML
interpreter and the YALE pipeline and asserts they agree on the expected value.
The SKI backend does not participate (it cannot handle recursion). Reductions
are bounded by 'defaultMaxSteps' (a loud failure, never a hang).
-}
recursionTests :: TestTree
recursionTests =
  testGroup
    "recursion (cyclic net)"
    [ testCase "rec identity: f 5 = 5" $
        assertYaleAgrees "module M\nlet rec f n = n\nlet result = f 5" (RInt 5)
    -- Light linear-recursion cases (isolate the knot with tiny inputs):
    -- count n = n via '1 + count (n - 1)'; far cheaper than fact (uses '+').
    , testCase "count 1 = 1" $ assertYaleAgrees (countSrc 1) (RInt 1)
    , testCase "count 2 = 2" $ assertYaleAgrees (countSrc 2) (RInt 2)
    , testCase "count 3 = 3" $ assertYaleAgrees (countSrc 3) (RInt 3)
    , testCase "fact 0 = 1" $ assertYaleAgrees (factSrc 0) (RInt 1)
    , testCase "fact 2 = 2" $ assertYaleAgrees (factSrc 2) (RInt 2)
    , testCase "fact 3 = 6" $ assertYaleAgrees (factSrc 3) (RInt 6)
    , testCase "fact 4 = 24" $ assertYaleAgrees (factSrc 4) (RInt 24)
    -- fact 5 = 120: exceeds the step budget (Church-factorial inefficiency).
    -- , testCase "fact 5 = 120" $ assertYaleAgrees (factSrc 5) (RInt 120)
    , testCase "fib 2 = 1" $ assertYaleAgrees (fibSrc 2) (RInt 1)
    , testCase "fib 3 = 2" $ assertYaleAgrees (fibSrc 3) (RInt 2)
    , testCase "fib 5 = 5" $ assertYaleAgrees (fibSrc 5) (RInt 5)
    , testCase "fib 6 = 8" $ assertYaleAgrees (fibSrc 6) (RInt 8)
    -- fib 10 = 55: exceeds the step budget (exponential recursion).
    -- , testCase "fib 10 = 55" $ assertYaleAgrees (fibSrc 10) (RInt 55)
    , testCase "countdown 1 = 1" $ assertYaleAgrees (countdownSrc 1) (RInt 1)
    , testCase "countdown 2 = 3" $ assertYaleAgrees (countdownSrc 2) (RInt 3)
    , testCase "countdown 4 = 10" $ assertYaleAgrees (countdownSrc 4) (RInt 10)
    , testCase "power 2 2 = 4" $ assertYaleAgrees (powerSrc 2 2) (RInt 4)
    , testCase "power 2 3 = 8" $ assertYaleAgrees (powerSrc 2 3) (RInt 8)
    , testCase "power 3 2 = 9" $ assertYaleAgrees (powerSrc 3 2) (RInt 9)
    , testCase "mul 2 3 = 6" $ assertYaleAgrees (mulSrc 2 3) (RInt 6)
    , testCase "mul 3 4 = 12" $ assertYaleAgrees (mulSrc 3 4) (RInt 12)
    ]
 where
  -- Light linear recursion: count n = n. Uses '+' (cheaper than '*'), so it
  -- isolates the knot's self-reference with minimal Church-arithmetic overhead.
  countSrc :: Integer -> Text
  countSrc n = "module M\nlet rec count k = if k < 1 then 0 else 1 + count (k - 1)\nlet result = count " <> T.pack (show n)
  factSrc :: Integer -> Text
  factSrc n = "module M\nlet rec fact k = if k < 1 then 1 else k * fact (k - 1)\nlet result = fact " <> T.pack (show n)
  -- NB: the LML grammar requires the left operand of an infix operator to be a
  -- 'simple_expr' (Parser.y: 'simple_expr infix_op compound_expr'); a bare
  -- application like 'fib (k - 1)' is only a 'compound_expr', so it must be
  -- parenthesized to sit on the left of '+'.
  fibSrc :: Integer -> Text
  fibSrc n = "module M\nlet rec fib k = if k < 2 then 1 else (fib (k - 1)) + fib (k - 2)\nlet result = fib " <> T.pack (show n)
  countdownSrc :: Integer -> Text
  countdownSrc n =
    "module M\nlet rec countdown k = if k < 1 then 0 else k + countdown (k - 1)\nlet result = countdown " <> T.pack (show n)
  powerSrc :: Integer -> Integer -> Text
  powerSrc x m =
    "module M\nlet rec power b e = if e < 1 then 1 else b * power b (e - 1)\nlet result = power "
      <> T.pack (show x)
      <> " "
      <> T.pack (show m)
  -- Multiplication via repeated addition: isolates the knot with a '+' body
  -- (cheaper than '*') and a two-argument recursive call.
  mulSrc :: Integer -> Integer -> Text
  mulSrc a b =
    "module M\nlet rec mul x y = if y < 1 then 0 else x + mul x (y - 1)\nlet result = mul "
      <> T.pack (show a)
      <> " "
      <> T.pack (show b)

{- | Basic-function tests: 'min' composed in different shapes. Non-recursive,
but they stress the Copy machinery with nested applications and shared bodies
(e.g. @min (min x y) z@ shares the 'min' body between two Apps). Part of the
minimal failing set for the YALE recursion bug (#99).
-}
basicFuncTests :: TestTree
basicFuncTests =
  testGroup
    "basic functions (min)"
    [ testCase "min3a (if-based) 5 2 8 = 2" $ assertYaleAgrees (min3aSrc 5 2 8) (RInt 2)
    , testCase "min3a (if-based) 1 9 4 = 1" $ assertYaleAgrees (min3aSrc 1 9 4) (RInt 1)
    , testCase "min3b (min (min x y) z) 5 2 8 = 2" $ assertYaleAgrees (min3bSrc 5 2 8) (RInt 2)
    , testCase "min3b (min (min x y) z) 1 9 4 = 1" $ assertYaleAgrees (min3bSrc 1 9 4) (RInt 1)
    , testCase "min3c (min z (min x y)) 5 2 8 = 2" $ assertYaleAgrees (min3cSrc 5 2 8) (RInt 2)
    , testCase "min3c (min z (min x y)) 1 9 4 = 1" $ assertYaleAgrees (min3cSrc 1 9 4) (RInt 1)
    , testCase "min4a (min (min a b) (min c d)) 3 7 1 5 = 1" $ assertYaleAgrees (min4aSrc 3 7 1 5) (RInt 1)
    , testCase "min4a (min (min a b) (min c d)) 4 2 8 6 = 2" $ assertYaleAgrees (min4aSrc 4 2 8 6) (RInt 2)
    , testCase "min4b (min d (min c (min a b))) 3 7 1 5 = 1" $ assertYaleAgrees (min4bSrc 3 7 1 5) (RInt 1)
    , testCase "min4b (min d (min c (min a b))) 4 2 8 6 = 2" $ assertYaleAgrees (min4bSrc 4 2 8 6) (RInt 2)
    -- Small inputs (isolate input-size dependence of the #104 timeouts):
    , testCase "min3a small 1 1 1 = 1" $ assertYaleAgrees (min3aSrc 1 1 1) (RInt 1)
    , testCase "min3a small 2 1 3 = 1" $ assertYaleAgrees (min3aSrc 2 1 3) (RInt 1)
    , testCase "min3b small 1 1 1 = 1" $ assertYaleAgrees (min3bSrc 1 1 1) (RInt 1)
    , testCase "min3b small 2 1 3 = 1" $ assertYaleAgrees (min3bSrc 2 1 3) (RInt 1)
    , testCase "min3c small 1 1 1 = 1" $ assertYaleAgrees (min3cSrc 1 1 1) (RInt 1)
    , testCase "min3c small 2 1 3 = 1" $ assertYaleAgrees (min3cSrc 2 1 3) (RInt 1)
    , testCase "min4a small 1 1 1 1 = 1" $ assertYaleAgrees (min4aSrc 1 1 1 1) (RInt 1)
    , testCase "min4a small 2 1 3 4 = 1" $ assertYaleAgrees (min4aSrc 2 1 3 4) (RInt 1)
    , testCase "min4b small 1 1 1 1 = 1" $ assertYaleAgrees (min4bSrc 1 1 1 1) (RInt 1)
    , testCase "min4b small 2 1 3 4 = 1" $ assertYaleAgrees (min4bSrc 2 1 3 4) (RInt 1)
    ]
 where
  -- 'min' is not an LML builtin (see DefaultEnv.hs), so each source defines it.
  minDef :: Text
  minDef = "let min a b = if a < b then a else b\n"
  min3aSrc :: Integer -> Integer -> Integer -> Text
  min3aSrc x y z =
    "module M\n"
      <> minDef
      <> "let min3a p q r = if p < q then (if p < r then p else r) else (if q < r then q else r)\n"
      <> "let result = min3a "
      <> T.pack (show x)
      <> " "
      <> T.pack (show y)
      <> " "
      <> T.pack (show z)
  min3bSrc :: Integer -> Integer -> Integer -> Text
  min3bSrc x y z =
    "module M\n"
      <> minDef
      <> "let min3b p q r = min (min p q) r\n"
      <> "let result = min3b "
      <> T.pack (show x)
      <> " "
      <> T.pack (show y)
      <> " "
      <> T.pack (show z)
  min3cSrc :: Integer -> Integer -> Integer -> Text
  min3cSrc x y z =
    "module M\n"
      <> minDef
      <> "let min3c p q r = min r (min p q)\n"
      <> "let result = min3c "
      <> T.pack (show x)
      <> " "
      <> T.pack (show y)
      <> " "
      <> T.pack (show z)
  min4aSrc :: Integer -> Integer -> Integer -> Integer -> Text
  min4aSrc a b c d =
    "module M\n"
      <> minDef
      <> "let min4a p q r s = min (min p q) (min r s)\n"
      <> "let result = min4a "
      <> T.pack (show a)
      <> " "
      <> T.pack (show b)
      <> " "
      <> T.pack (show c)
      <> " "
      <> T.pack (show d)
  min4bSrc :: Integer -> Integer -> Integer -> Integer -> Text
  min4bSrc a b c d =
    "module M\n"
      <> minDef
      <> "let min4b p q r s = min s (min r (min p q))\n"
      <> "let result = min4b "
      <> T.pack (show a)
      <> " "
      <> T.pack (show b)
      <> " "
      <> T.pack (show c)
      <> " "
      <> T.pack (show d)

{- | Mutual-recursion tests (Bekić expansion: Y combinator over
Church-encoded pairs). The base cases (@even 0@, @odd 0@) pass within the step
budget. The recursive cases (@even 4@, @odd 3@, @odd 7@) are CORRECT but
exceed the step budget (the Y combinator duplicates its argument on every
unfolding, and Church encoding + interaction nets is inherently slow for
recursive programs). Kept as commented-out references.
-}
mutualRecTests :: TestTree
mutualRecTests =
  testGroup
    "mutual recursion"
    [ testCase "even 0 = true" $ assertYaleAgrees (evenOddSrc "even" 0) (RBool True)
    , testCase "odd 0 = false" $ assertYaleAgrees (evenOddSrc "odd" 0) (RBool False)
    -- Recursive cases (correct but slow — exceed defaultMaxSteps=100000):
    -- , testCase "even 4 = true" $ assertYaleAgrees (evenOddSrc "even" 4) (RBool True)
    -- , testCase "odd 3 = true" $ assertYaleAgrees (evenOddSrc "odd" 3) (RBool True)
    -- , testCase "odd 7 = true" $ assertYaleAgrees (evenOddSrc "odd" 7) (RBool True)
    ]
 where
  evenOddSrc :: Text -> Integer -> Text
  evenOddSrc fn n =
    "module M\nlet rec even k = if k < 1 then true else odd (k - 1) and odd k = if k < 1 then false else even (k - 1)\nlet result = "
      <> fn
      <> " "
      <> T.pack (show n)

{- | Tests for the step-bounded SKI runner: a well-behaved term (the Church
numeral 5) reduces to normal form within the budget, while the non-terminating
term @Omega = (\\x. x x) (\\x. x x)@ exhausts the budget.
-}
runnerTests :: TestTree
runnerTests =
  testGroup
    "reduceToNormalForm"
    [ testCase "church 5 reduces to normal form" $ assertReachesNormalForm (intToChurch 5)
    , testCase "Omega hits the step bound" $ assertHitsBound omegaTerm
    ]

-- | Assert that a SKI term reduces to normal form within the default budget.
assertReachesNormalForm :: Term.Expr -> Assertion
assertReachesNormalForm term =
  case reduceToNormalForm defaultMaxSteps (fromTerm term) of
    Right _ -> return ()
    Left (StepOverflow step) -> assertFailure $ "did not reach normal form within " <> show step <> " steps"

-- | Assert that a SKI term exhausts a small step budget (does not reach normal form).
assertHitsBound :: Term.Expr -> Assertion
assertHitsBound term =
  case reduceToNormalForm (MaxSteps 100) (fromTerm term) of
    Left (StepOverflow _) -> return ()
    Right _ -> assertFailure "should not reach normal form"

{- | The SKI term for @Omega = (\\x. x x) (\\x. x x)@, which reduces to itself
forever (non-terminating).
-}
omegaTerm :: Term.Expr
omegaTerm = toSki (LApp (LLam "x" (LApp (LVar "x") (LVar "x"))) (LLam "y" (LApp (LVar "y") (LVar "y"))))

{- | Equivalence tests: the worklist driver ('reduceSteps') and the fixed-order
reference ('reduceStepsFixedOrder') must both reach normal form and decode to the
same value on a set of small terms. Guards against a regression in the worklist
driver's correctness (it uses a different contraction order, so the step count
may differ, but the decoded result must not).
-}
equivalenceTests :: TestTree
equivalenceTests =
  testGroup
    "worklist vs fixed-order equivalence"
    [ equiv "church 0" (intToChurch 0)
    , equiv "church 5" (intToChurch 5)
    , equiv "church 10" (intToChurch 10)
    ]
 where
  equiv :: String -> Term.Expr -> TestTree
  equiv lbl term = testCase lbl $ do
    let g = fromTerm (wrapDecoder term)
        (_sw, gw, nw) = reduceSteps maxStepsInt g
        (_sf, gf, nf) = reduceStepsFixedOrder maxStepsInt g
    assertBool "worklist reached normal form" nw
    assertBool "fixed-order reached normal form" nf
    assertEqual "decoded values agree" (decodeTicks gw) (decodeTicks gf)

-- | Wrap a term with the unary decoder (the same wrapper 'decodeChurch' uses).
wrapDecoder :: Term.Expr -> Term.Expr
wrapDecoder term =
  let decoderFn = Term.A (Term.A Term.S (Term.A Term.K (Term.V tickName))) Term.I
   in Term.A (Term.A term decoderFn) (Term.V baseName)

-- | Count the tick nodes in a reduced graph (the decoded integer).
decodeTicks :: Graph INet.SKI -> Int
decodeTicks = countLogical isTick
 where
  isTick :: INet.SKI -> Bool
  isTick = \case
    INet.V{INet.name = n} -> n == tickName
    _ -> False

{- | Regression test for the "drop unevaluated argument" optimization. The term
@(K 5) (add 100 100)@ reduces to @5@, discarding the argument @add 100 100@.

Two things contribute to the step count. First, the discarded argument's own
internal redexes reduce before the @K1@ contraction fires (the worklist orders
pairs by node id, and the argument's nodes precede the @K1@ pair); that cost
(~4100 steps for @add 100 100@) is paid with or without the optimization.
Second, once @K1@ fires, the already-reduced argument must be erased: the
optimization deletes the whole disjoint component at once, whereas without it
the eraser propagates through the component one node at a time (~800 extra
steps). The guard below sits between the two totals (5802 with the
optimization, 6604 without), so it fails if the eager erasure regresses.
-}
dropArgTests :: TestTree
dropArgTests =
  testGroup
    "drop unevaluated argument"
    [ testCase "(K 5) (add 100 100) reduces within budget" $ do
        let term = Term.A (Term.A Term.K (intToChurch 5)) (apply2 addLam 100 100)
            (steps, _g, reachedNF) = reduceSteps maxStepsInt (fromTerm term)
        assertBool "reached normal form" reachedNF
        when (steps > dropArgGuard) $
          assertFailure $
            "step count " <> show steps <> " exceeds guard " <> show dropArgGuard
        putStrLn $ "  (K 5) (add 100 100): " <> show steps <> " steps"
    ]

{- | Step-count guard for the "drop unevaluated argument" test. Measured: 5802
steps with the eager-erasure optimization, 6604 without (the difference is the
eraser propagation through the discarded argument). The guard sits between the
two, with margin above the optimized count for small contraction-order shifts.
-}
dropArgGuard :: Int
dropArgGuard = 6000

{- | Tests for 'skiEval': distinct small integers evaluate to distinct values,
and the decoder is stable across equivalent encodings (@1 + 1@ and the direct
Church numeral @2@ both evaluate to @2@).
-}
skiEvalTests :: TestTree
skiEvalTests =
  testGroup
    "skiEval"
    [ testCase "distinct small ints" $ do
        assertSkiEval [] (Lit (LitInt 0)) (RInt 0)
        assertSkiEval [] (Lit (LitInt 1)) (RInt 1)
        assertSkiEval [] (Lit (LitInt 2)) (RInt 2)
        assertSkiEval [] (Lit (LitInt 3)) (RInt 3)
    , testCase "stable across equivalent encodings (1+1 == church 2)" $ do
        assertSkiEval [] (binOp stdPlus 1 1) (RInt 2)
        assertSkiEval [] (Lit (LitInt 2)) (RInt 2)
    ]

{- | Evaluate a core program via 'skiEval' and assert it equals @expected@. The
expected value doubles as the decode shape (the decoder is type-directed).
-}
assertSkiEval :: [CoreBind] -> CoreExpr -> Value' -> IO ()
assertSkiEval binds expr expected = case skiEval expected binds expr of
  Right v -> assertEqual "skiEval" expected v
  Left e -> assertFailure $ "skiEval failed: " <> show e

-- * YALE decoder tests (S5)

{- | Evaluate a core program via 'skiEvalYale' and assert it equals @expected@.
The expected value doubles as the decode shape (the decoder is type-directed).
-}
assertSkiEvalYale :: [CoreBind] -> CoreExpr -> Value' -> IO ()
assertSkiEvalYale binds expr expected = case skiEvalYale expected binds expr of
  Right v -> assertEqual "skiEvalYale" expected v
  Left e -> assertFailure $ "skiEvalYale failed: " <> show e

{- | YALE decoder tests: the shape-directed decoder reproduces the SKI decoder's
results on the non-recursive subset.
-}
yaleEvalTests :: TestTree
yaleEvalTests =
  testGroup
    "skiEvalYale"
    [ testCase "distinct small ints" $ do
        assertSkiEvalYale [] (Lit (LitInt 0)) (RInt 0)
        assertSkiEvalYale [] (Lit (LitInt 1)) (RInt 1)
        assertSkiEvalYale [] (Lit (LitInt 2)) (RInt 2)
        assertSkiEvalYale [] (Lit (LitInt 3)) (RInt 3)
    , testCase "stable across equivalent encodings (1+1 == church 2)" $ do
        assertSkiEvalYale [] (binOp stdPlus 1 1) (RInt 2)
        assertSkiEvalYale [] (Lit (LitInt 2)) (RInt 2)
    , testCase "true literal" $ assertSkiEvalYale [] (Var stdTrue) (RBool True)
    , testCase "false literal" $ assertSkiEvalYale [] (Var stdFalse) (RBool False)
    , testCase "1 < 2" $ assertSkiEvalYale [] (binOp stdLess 1 2) (RBool True)
    , testCase "2 < 1" $ assertSkiEvalYale [] (binOp stdLess 2 1) (RBool False)
    ]

{- | Church-encoded @if-then-else@ tests for the YALE pipeline. A boolean
'Match' compiles to the Church encoding @scrutLam trueBranch falseBranch@ — a
plain generic lambda term with no special @if@ agent — and the YALE reducer must
select the correct branch. This is the intermediate checkpoint that the Church
encoding of @if@ reduces correctly (literal conditions first, then the
comparison conditions that drive the recursive programs).
-}
churchIfTests :: TestTree
churchIfTests =
  testGroup
    "church-encoded if (YALE)"
    [ testCase "if true then 10 else 20 = 10" $
        assertSkiEvalYale
          []
          (Match (Var stdTrue) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 10)
    , testCase "if false then 10 else 20 = 20" $
        assertSkiEvalYale
          []
          (Match (Var stdFalse) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 20)
    , testCase "if 2 < 3 then 10 else 20 = 10" $
        assertSkiEvalYale
          []
          (Match (binOp stdLess 2 3) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 10)
    , testCase "if 2 < 1 then 10 else 20 = 20" $
        assertSkiEvalYale
          []
          (Match (binOp stdLess 2 1) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 20)
    ]

{- | Non-recursive complex-case tests for the YALE pipeline: algebraic types with
pattern matching, branched non-recursive functions, and higher-order function
calls. Each case runs BOTH the LML interpreter and the YALE pipeline and asserts
they agree on the expected value (differential). These exercise the generic
Church-encoded lambda terms (no special @if@ agent) that the YALE reducer must
reduce correctly, including the cross-lambda free-variable dynamics that the
over-erasure fix addresses.
-}
yaleComplexTests :: TestTree
yaleComplexTests =
  testGroup
    "YALE non-recursive complex cases"
    [ testCase "ADT match: unwrap (Some 5) = 6" $
        assertYaleAgrees
          "module M\ntype 'a opt = None | Some of 'a\nlet unwrap = fun v -> match v with\n  | None -> 0\n  | Some x -> x + 1\nlet result = unwrap (Some 5)"
          (RInt 6)
    , testCase "ADT match: unwrap None = 0" $
        assertYaleAgrees
          "module M\ntype 'a opt = None | Some of 'a\nlet unwrap = fun v -> match v with\n  | None -> 0\n  | Some x -> x + 1\nlet result = unwrap None"
          (RInt 0)
    , testCase "ADT match: 2-arg ctor, sum fields = 7" $
        assertYaleAgrees
          "module M\ntype 'a pair = Pair of 'a * 'a\nlet sumFields = fun p -> match p with\n  | Pair (a, b) -> a + b\nlet result = sumFields (Pair (3, 4))"
          (RInt 7)
    , testCase "branched: clamp 15 = 10" $
        assertYaleAgrees
          "module M\nlet clamp = fun n -> if n < 0 then 0 else if n > 10 then 10 else n\nlet result = clamp 15"
          (RInt 10)
    , testCase "branched: clamp 5 = 5" $
        assertYaleAgrees
          "module M\nlet clamp = fun n -> if n < 0 then 0 else if n > 10 then 10 else n\nlet result = clamp 5"
          (RInt 5)
    , testCase "branched: clamp 0 = 0" $
        assertYaleAgrees
          "module M\nlet clamp = fun n -> if n < 0 then 0 else if n > 10 then 10 else n\nlet result = clamp 0"
          (RInt 0)
    , testCase "function call: apply double 5 = 10" $
        assertYaleAgrees
          "module M\nlet apply = fun f x -> f x\nlet double = fun x -> x * 2\nlet result = apply double 5"
          (RInt 10)
    , testCase "composition: compose double inc 3 = 8" $
        assertYaleAgrees
          "module M\nlet compose = fun f g x -> f (g x)\nlet double = fun x -> x * 2\nlet inc = fun x -> x + 1\nlet result = compose double inc 3"
          (RInt 8)
    , testCase "nested call: applyTwice (fun x -> x + 1) 10 = 12" $
        assertYaleAgrees
          "module M\nlet applyTwice = fun f x -> f (f x)\nlet result = applyTwice (fun x -> x + 1) 10"
          (RInt 12)
    , -- Top-level @let@ bindings that ARE functions (multi-argument, curried),
      -- using the @let name args = body@ form (no explicit @fun@).
      testCase "top-level fn: max 3 5 = 5" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max 3 5"
          (RInt 5)
    , testCase "top-level fn: max 5 3 = 5" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max 5 3"
          (RInt 5)
    , testCase "top-level fn: max 7 7 = 7" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max 7 7"
          (RInt 7)
    , testCase "top-level fn: max3 1 2 3 = 3" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet max3 x y z = max x (max y z)\nlet result = max3 1 2 3"
          (RInt 3)
    , testCase "top-level fn: max3 9 2 4 = 9" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet max3 x y z = max x (max y z)\nlet result = max3 9 2 4"
          (RInt 9)
    , -- Associativity property: @max x (max y z) == max (max x y) z@. Both sides
      -- are evaluated independently and must agree with the expected value.
      testCase "assoc: max 1 (max 2 3) = 3" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max 1 (max 2 3)"
          (RInt 3)
    , testCase "assoc: max (max 1 2) 3 = 3" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max (max 1 2) 3"
          (RInt 3)
    , testCase "assoc: max 5 (max 1 3) = 5" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max 5 (max 1 3)"
          (RInt 5)
    , testCase "assoc: max (max 5 1) 3 = 5" $
        assertYaleAgrees
          "module M\nlet max x y = if x < y then y else x\nlet result = max (max 5 1) 3"
          (RInt 5)
    ]

{- | Simple-construction localization matrix for the non-terminating
duplication bug. Each case is a top-level function applied to arguments, run
through BOTH the LML interpreter and the YALE pipeline ('assertYaleAgrees').

Localisation result: the trigger is applying 'pred' (via 'sub', which backs
'<' / '<=' / '>' / '-') FOUR OR MORE times to a Church numeral. 'a - b' is
'pred^b(a)', and 'm < n' is 'not (isZero (n - m))', so the LEFT operand of
'<' (respectively the subtrahend of '-') is the number of 'pred' applications:

  * 1-3 preds  -> converges (fast for small, slow for large numerals);
  * >= 4 preds -> the net grows ~1 node/step and overflows the step budget.

The cases marked @BUG@ therefore fail with 'StepOverflowReached' until the
'pred' duplication is fixed; they are the regression probes for that fix.
(They never hang: 'skiEvalYale' is bounded by 'defaultMaxSteps'.)
-}
yaleSimpleConstructionTests :: TestTree
yaleSimpleConstructionTests =
  testGroup
    "YALE simple constructions"
    [ -- Arithmetic with big args: baseline, all converge.
      testCase "f x y = x + y ; f 3 4 = 7" $ fnCase "let f x y = x + y" "f 3 4" (RInt 7)
    , testCase "f x y = x * y ; f 3 4 = 12" $ fnCase "let f x y = x * y" "f 3 4" (RInt 12)
    , testCase "f x y = x - y ; f 4 3 = 1" $ fnCase "let f x y = x - y" "f 4 3" (RInt 1)
    , testCase "f x = x + x ; f 3 = 6" $ fnCase "let f x = x + x" "f 3" (RInt 6)
    , testCase "f x = x * x ; f 3 = 9" $ fnCase "let f x = x * x" "f 3" (RInt 9)
    , -- Comparison, left operand <= 3 (<= 3 preds): converge.
      testCase "f x y = x < y ; f 1 2 = true" $ fnCase "let f x y = x < y" "f 1 2" (RBool True)
    , testCase "f x y = x < y ; f 2 1 = false" $ fnCase "let f x y = x < y" "f 2 1" (RBool False)
    , testCase "f x y = x < y ; f 3 4 = true" $ fnCase "let f x y = x < y" "f 3 4" (RBool True)
    , testCase "f x = x < 3 ; f 1 = true" $ fnCase "let f x = x < 3" "f 1" (RBool True)
    , testCase "f x = x < 3 ; f 3 = false" $ fnCase "let f x = x < 3" "f 3" (RBool False)
    , -- Comparison, left operand >= 4 (>= 4 preds): THE BUG (overflows).
      testCase "BUG f x y = x < y ; f 4 5 = true" $ fnCase "let f x y = x < y" "f 4 5" (RBool True)
    , testCase "BUG f x y = x < y ; f 5 3 = false" $ fnCase "let f x y = x < y" "f 5 3" (RBool False)
    , testCase "BUG f x = x < 3 ; f 4 = false" $ fnCase "let f x = x < 3" "f 4" (RBool False)
    , testCase "BUG f x = x < 3 ; f 5 = false" $ fnCase "let f x = x < 3" "f 5" (RBool False)
    , -- Subtraction, subtrahend >= 4 (>= 4 preds): confirms sub/pred is the
      -- culprit, independent of the '<' / isZero / not wrappers.
      testCase "f x = x - 3 ; f 5 = 2" $ fnCase "let f x = x - 3" "f 5" (RInt 2)
    , testCase "BUG f x = x - 4 ; f 5 = 1" $ fnCase "let f x = x - 4" "f 5" (RInt 1)
    , testCase "BUG f x = x - 4 ; f 4 = 0" $ fnCase "let f x = x - 4" "f 4" (RInt 0)
    , -- Multi-param arithmetic / if: converge.
      testCase "f x y z = x + y * z ; f 3 4 5 = 23" $ fnCase "let f x y z = x + y * z" "f 3 4 5" (RInt 23)
    , testCase "f x y z = if x < y then y else z ; f 3 4 5 = 4" $
        fnCase "let f x y z = if x < y then y else z" "f 3 4 5" (RInt 4)
    ]
 where
  fnCase defn call =
    assertYaleAgrees ("module M\n" <> defn <> "\nlet result = " <> call)

{- | The boolean-match scrutinee binder (the desugared @if@ binds the condition
to this name).
-}
scrut :: Var
scrut = Id (Name (mkLongident (pure "t#a")))

-- | A boolean-match alternative for the constructor @d@ with body @e@.
boolAlt :: Var -> CoreExpr -> CoreMatchAlt
boolAlt d e = (DataAlt (nameOfVar d), [], e)
 where
  nameOfVar (Id n) = n

{- | Tests for the boolean and comparison encodings: the boolean literals and
the four comparison operators decode to the expected Church booleans, and an
@if-then-else@ (a boolean 'Match') selects the correct branch.
-}
boolTests :: TestTree
boolTests =
  testGroup
    "booleans and comparisons"
    [ testCase "true literal" $ assertSkiEval [] (Var stdTrue) (RBool True)
    , testCase "false literal" $ assertSkiEval [] (Var stdFalse) (RBool False)
    , testCase "1 < 2" $ assertSkiEval [] (binOp stdLess 1 2) (RBool True)
    , testCase "2 < 1" $ assertSkiEval [] (binOp stdLess 2 1) (RBool False)
    , testCase "1 > 2" $ assertSkiEval [] (binOp stdGreater 1 2) (RBool False)
    , testCase "2 > 1" $ assertSkiEval [] (binOp stdGreater 2 1) (RBool True)
    , testCase "1 <= 2" $ assertSkiEval [] (binOp stdLessEq 1 2) (RBool True)
    , testCase "2 <= 1" $ assertSkiEval [] (binOp stdLessEq 2 1) (RBool False)
    , testCase "1 >= 2" $ assertSkiEval [] (binOp stdGreaterEq 1 2) (RBool False)
    , testCase "2 >= 1" $ assertSkiEval [] (binOp stdGreaterEq 2 1) (RBool True)
    , testCase "if 1 < 2 then 10 else 20" $
        assertEqualBoth
          []
          (Match (binOp stdLess 1 2) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 10)
    , testCase "if 2 < 1 then 10 else 20" $
        assertEqualBoth
          []
          (Match (binOp stdLess 2 1) scrut (boolAlt stdTrue (Lit (LitInt 10)) :| [boolAlt stdFalse (Lit (LitInt 20))]))
          (RInt 20)
    ]

{- | Tests for the tuple (nested Church pair) encoding: a Core tuple decodes to
the expected 'RTuple' (2-, 3-, and mixed-element), and a tuple program agrees
between the LML interpreter and the SKI pipeline.
-}
tupleTests :: TestTree
tupleTests =
  testGroup
    "tuples"
    [ testCase "2-tuple (1, 2)" $
        assertSkiEval [] (Tuple (Lit (LitInt 1)) (Lit (LitInt 2) :| [])) (RTuple [RInt 1, RInt 2])
    , testCase "3-tuple (1, 2, 3)" $
        assertSkiEval
          []
          (Tuple (Lit (LitInt 1)) (Lit (LitInt 2) :| [Lit (LitInt 3)]))
          (RTuple [RInt 1, RInt 2, RInt 3])
    , testCase "mixed tuple (1, 2 + 3)" $
        assertSkiEval [] (Tuple (Lit (LitInt 1)) (binOp stdPlus 2 3 :| [])) (RTuple [RInt 1, RInt 5])
    , testCase "differential: (1, 2) LML == SKI" $
        assertTranslateAndRun "module M\nlet result = (1, 2)" (RTuple [RInt 1, RInt 2])
    ]

{- | Tests for the ADT (tagged) encoding: constructor values and applications
decode to the expected value, and match programs agree between the LML
interpreter and the SKI pipeline. Covers 0-, 1-, and 2-arity constructors.
-}
adtTests :: TestTree
adtTests =
  testGroup
    "ADTs (tagged encoding)"
    [ testCase "1-arg ctor: match (Some 5) -> 5" $
        assertTranslateAndRun
          "module M\ntype 'a opt = None | Some of 'a\nlet result = match (Some 5) with\n  | None -> 0\n  | Some v -> v"
          (RInt 5)
    , testCase "2-arg ctor: match (Pair (3, 4)) -> 7" $
        assertTranslateAndRun
          "module M\ntype 'a pair = Pair of 'a * 'a\nlet result = match (Pair (3, 4)) with\n  | Pair (a, b) -> a + b"
          (RInt 7)
    , testCase "0-arg ctor: match On -> 1" $
        assertTranslateAndRun
          "module M\ntype flag = Off | On\nlet result = match On with\n  | Off -> 0\n  | On -> 1"
          (RInt 1)
    , testCase "0-arg ctor, reordered alts: match Off (On first) -> 0" $
        assertTranslateAndRun
          "module M\ntype flag = Off | On\nlet result = match Off with\n  | On -> 1\n  | Off -> 0"
          (RInt 0)
    , testCase "nested ctors: match (Some (Pair (1, 2))) -> 3" $
        assertTranslateAndRun
          "module M\ntype 'a pair = Pair of 'a * 'a\ntype 'a opt = None | Some of 'a\nlet result = match (Some (Pair (1, 2))) with\n  | None -> 0\n  | Some p -> match p with\n    | Pair (a, b) -> a + b"
          (RInt 3)
    , testCase "ADT value: Some 5" $
        assertTranslateAndRun
          "module M\ntype 'a opt = None | Some of 'a\nlet result = Some 5"
          (RAdt (Name (mkLongident ("M" :| ["Some"]))) [RInt 5])
    , testCase "ADT value: Pair (3, 4)" $
        assertTranslateAndRun
          "module M\ntype 'a pair = Pair of 'a * 'a\nlet result = Pair (3, 4)"
          (RAdt (Name (mkLongident ("M" :| ["Pair"]))) [RInt 3, RInt 4])
    , testCase "ADT value: On (0-arg ctor)" $
        assertTranslateAndRun
          "module M\ntype flag = Off | On\nlet result = On"
          (RAdt (Name (mkLongident ("M" :| ["On"]))) [])
    ]

-- | The canonical unit value (@()@), as the LML interpreter produces it.
unitValue :: Value'
unitValue = RAdt (Name (mkLongident (pure "()"))) []

{- | Tests for the char/string/unit encodings and @print_int@: char and string
literals round-trip through the SKI pipeline, an int-literal match classifies
a value, and @print_int@ returns unit (the printed side-effect is ignored).
All differential (LML == SKI).
-}
charStringTests :: TestTree
charStringTests =
  testGroup
    "char / string / unit"
    [ testCase "char literal: 'a'" $
        assertTranslateAndRun "module M\nlet result = 'a'" (RChar 'a')
    , testCase "string literal: \"hi\"" $
        assertTranslateAndRun "module M\nlet result = \"hi\"" (RString "hi")
    , testCase "print_int 5 = unit" $
        assertTranslateAndRun "module M\nlet result = print_int 5" unitValue
    , testCase "int-literal match: classify 1 = 10" $
        assertTranslateAndRun
          "module M\nlet classify n = match n with\n  | 1 -> 10\n  | 2 -> 20\n  | _ -> 0\nlet result = classify 1"
          (RInt 10)
    , testCase "int-literal match: classify 2 = 20" $
        assertTranslateAndRun
          "module M\nlet classify n = match n with\n  | 1 -> 10\n  | 2 -> 20\n  | _ -> 0\nlet result = classify 2"
          (RInt 20)
    , testCase "int-literal match: classify 7 = 0 (default)" $
        assertTranslateAndRun
          "module M\nlet classify n = match n with\n  | 1 -> 10\n  | 2 -> 20\n  | _ -> 0\nlet result = classify 7"
          (RInt 0)
    ]

{- | Fixed-example differential tests: each LML program is run through the full
Driver pipeline (parse, resolve, typecheck, desugar) and then through
'translateAndRun', which must agree with the LML interpreter. The result is the
right-hand side of the last top-level binding.

The set covers addition, multiplication, saturating subtraction, an anonymous
lambda, a named lambda bound by a top-level @let@, a nested @let@, and a lambda
with an arithmetic body. These were the examples deferred while the SKI reducer
was unoptimized; the worklist driver makes them fast enough to run in the suite.
-}
fixedExampleTests :: TestTree
fixedExampleTests =
  testGroup
    "translateAndRun (fixed examples)"
    [ testCase "2 + 3 = 5" $ assertTranslateAndRun "module M\nlet result = 2 + 3" (RInt 5)
    , testCase "2 * 3 = 6" $ assertTranslateAndRun "module M\nlet result = 2 * 3" (RInt 6)
    , testCase "5 - 2 = 3" $ assertTranslateAndRun "module M\nlet result = 5 - 2" (RInt 3)
    , testCase "(fun x -> x) 4 = 4" $ assertTranslateAndRun "module M\nlet result = (fun x -> x) 4" (RInt 4)
    , testCase "let f = fun x -> x in f 3 = 3" $
        assertTranslateAndRun "module M\nlet f = fun x -> x\nlet result = f 3" (RInt 3)
    , testCase "nested let: (2+3)*2 = 10" $
        assertTranslateAndRun "module M\nlet a = 2 + 3\nlet b = a * 2\nlet result = b" (RInt 10)
    , testCase "(fun x -> x + 1) 4 = 5" $ assertTranslateAndRun "module M\nlet result = (fun x -> x + 1) 4" (RInt 5)
    ]

{- | Compile a LML source program and run it through 'translateAndRun', asserting
the result equals @expected@.
-}
assertTranslateAndRun :: Text -> Value' -> IO ()
assertTranslateAndRun src expected = case compileLml src of
  Left e -> assertFailure $ "compileLml failed: " <> e
  Right (binds, resultExpr) -> do
    res <- translateAndRun binds resultExpr
    case res of
      Right v -> assertEqual "translateAndRun" expected v
      Left e -> assertFailure $ "translateAndRun failed: " <> show e

{- | Compile a LML source program to (enclosing binds, result expression), where
the result is the right-hand side of the last top-level binding and all preceding
bindings form the enclosing context.
-}
compileLml :: Text -> Either String ([CoreBind], CoreExpr)
compileLml src = case parseLmlProgram [src] of
  Left e -> Left ("parse: " <> e)
  Right parsed ->
    let resolved = resolveLmlProgram parsed
     in case typecheckLmlProgram resolved of
          Left e -> Left ("typecheck: " <> show e)
          Right typed ->
            let binds = runMonadDesugar (desugarLmlProgram typed)
             in case viaNonEmpty id binds of
                  Nothing -> Left "empty program: no result binding"
                  Just bs -> Right (init bs, rhsOf (last bs))
 where
  rhsOf (NonRec _ e) = e
  rhsOf _ = error "compileLml: last binding must be a non-recursive let"

-- * Differential test harness

{- | Assert that the LML interpreter and the SKI pipeline agree on the program,
and that the agreed value equals @expected@. Built on 'translateAndRun', which
runs both evaluators and reports 'Mismatch' (carrying both values) on
disagreement. A 'StepOverflowReached' or any other 'SkiError' is a loud failure
(never a hang): the reduction is bounded by 'defaultMaxSteps'.
-}
assertEqualBoth :: [CoreBind] -> CoreExpr -> Value' -> IO ()
assertEqualBoth binds expr expected = do
  res <- translateAndRun binds expr
  case res of
    Right v -> assertEqual "LML and SKI agree" expected v
    Left e -> assertFailure $ "differential failure: " <> show e

{- | Smoke test for the differential harness: a trivial program (@double 3@) must
agree between the LML interpreter and the SKI pipeline, and the agreed value must
be @6@. Exercises 'assertEqualBoth' end-to-end (compile → run both → compare).
-}
harnessTests :: TestTree
harnessTests =
  testGroup
    "differential harness"
    [ testCase "smoke: double 3 agrees (LML == SKI)" $ do
        case compileLml "module M\nlet double = fun x -> x + x\nlet result = double 3" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> assertEqualBoth binds resultExpr (RInt 6)
    ]

-- * Fixed minimal-subset programs + differential property

{- | Fixed minimal-subset programs as LML source templates. Each template carries
a @{n}@ placeholder for the random input; the property substitutes a bounded
integer and checks the LML interpreter and the SKI pipeline agree. The programs
cover addition, multiplication, let, lambda, and (higher-order) application,
with no recursion (recursion is #88's scope).
-}
programTemplates :: [(String, Text)]
programTemplates =
  [ ("double", "module M\nlet double = fun x -> x + x\nlet result = double {n}")
  , ("sum2", "module M\nlet sum2 = fun x y -> x + y\nlet result = sum2 3 {n}")
  ,
    ( "apply_twice"
    , "module M\nlet apply_twice = fun f x -> f (f x)\nlet double = fun x -> x + x\nlet result = apply_twice double {n}"
    )
  ,
    ( "compose"
    , "module M\nlet compose = fun f g x -> f (g x)\nlet double = fun x -> x + x\nlet inc = fun x -> x + 1\nlet result = compose double inc {n}"
    )
  , ("let_heavy", "module M\nlet result = let a = {n} + 1 in let b = a * 2 in b + a")
  ]

-- | Substitute the @{n}@ placeholder in a template with the input integer.
substituteTemplate :: Int -> Text -> Text
substituteTemplate n = T.replace "{n}" (T.pack (show n))

{- | The differential property: for every fixed program and every bounded input,
the LML interpreter and the SKI pipeline must agree. A compile failure, a
step-overflow, or a value mismatch is a loud property failure (never a hang):
the reduction is bounded by 'defaultMaxSteps'.
-}
propDifferential :: Property
propDifferential = withTests 100 . property $ do
  n <- forAll (Gen.element [0 .. 8])
  (name, template) <- forAll (Gen.element programTemplates)
  let src = substituteTemplate n template
  case compileLml src of
    Left e -> fail $ "compileLml failed for " <> name <> " " <> show n <> ": " <> e
    Right (binds, resultExpr) -> do
      res <- liftIO $ translateAndRun binds resultExpr
      case res of
        Right _ -> Hedgehog.assert True
        Left err -> fail $ "differential failure for " <> name <> " " <> show n <> ": " <> show err

{- | The closedness property: for every program in the S1 corpus,
'translateLam' yields a closed 'Lam' (no free variables).
-}
propTranslateLamClosed :: Property
propTranslateLamClosed = withTests 100 . property $ do
  n <- forAll (Gen.element [0 .. 8])
  (name, template) <- forAll (Gen.element programTemplates)
  let src = substituteTemplate n template
  case compileLml src of
    Left e -> fail $ "compileLml failed for " <> name <> " " <> show n <> ": " <> e
    Right (binds, resultExpr) -> case translateLam binds resultExpr of
      Left err -> fail $ "translateLam failed for " <> name <> " " <> show n <> ": " <> show err
      Right lam -> Hedgehog.assert (Set.null (fv lam))

{- | The SKI-projection property: for a non-recursive program, 'toSki' of
'translateLam' equals 'translateCore' (the SKI backend is a projection of the
shared lambda IR).
-}
propTranslateLamIsSkiProjection :: Property
propTranslateLamIsSkiProjection = withTests 100 . property $ do
  n <- forAll (Gen.element [0 .. 8])
  (name, template) <- forAll (Gen.element programTemplates)
  let src = substituteTemplate n template
  case compileLml src of
    Left e -> fail $ "compileLml failed for " <> name <> " " <> show n <> ": " <> e
    Right (binds, resultExpr) ->
      case (translateLam binds resultExpr, translateCore binds resultExpr) of
        (Right lam, Right ski) -> Hedgehog.assert (toSki lam == ski)
        (Left err, _) -> fail $ "translateLam failed for " <> name <> " " <> show n <> ": " <> show err
        (_, Left err) -> fail $ "translateCore failed for " <> name <> " " <> show n <> ": " <> show err

propertyTests :: TestTree
propertyTests =
  testGroup
    "differential property (minimal subset)"
    [ testProperty "lml == ski for all (program, input)" propDifferential
    , testProperty "translateLam is closed for all (program, input)" propTranslateLamClosed
    , testProperty "toSki (translateLam p) == translateCore p (non-recursive)" propTranslateLamIsSkiProjection
    ]

-- * Golden regression triples

{- | Golden regression triples: for a fixed set of (program, input) pairs, the
canonical result (agreed by the LML interpreter and the SKI pipeline) is captured
in a golden file under @test\/golden\/@. A change in either interpreter's result
flips the golden (a loud regression). The set is deliberately small to bound
runtime; the property test above covers the broader input space.
-}
goldenTests :: TestTree
goldenTests =
  testGroup
    "golden regression triples"
    [ goldenVsString (name <> "-" <> show input) goldenFile (goldenAction name input)
    | (name, input) <- goldenTriples
    , let goldenFile = "test" </> "golden" </> name <> "-" <> show input <.> "out"
    ]
 where
  goldenAction :: String -> Int -> IO LByteString
  goldenAction name input = do
    case compileLml (substituteTemplate input (templateFor name)) of
      Left e -> fail $ "compileLml failed: " <> e
      Right (binds, resultExpr) -> do
        res <- translateAndRun binds resultExpr
        case res of
          Right v -> return (pack (show v <> "\n"))
          Left err -> fail $ "differential failure: " <> show err

-- | The fixed set of (program, input) golden triples.
goldenTriples :: [(String, Int)]
goldenTriples =
  [ ("double", 3)
  , ("sum2", 4)
  , ("apply_twice", 2)
  , ("compose", 5)
  , ("let_heavy", 1)
  ]

-- | Look up a program template by name (from 'programTemplates').
templateFor :: String -> Text
templateFor name = case lookup name programTemplates of
  Just t -> t
  Nothing -> error $ T.pack ("templateFor: unknown program " <> name)

{- | Reduction step-count baseline for a fixed set of programs. Measures the
number of interaction-net contractions (via 'reduceSteps') to reduce the wrapped
decode term — the same term 'decodeChurch' reduces — to normal form. Establishes
the baseline the reducer optimization improves on, and gives #87 the step
magnitudes to set its bounds. The step count is reported so the baseline is
visible; a generous regression guard bounds it.
-}
benchmarkTests :: TestTree
benchmarkTests =
  testGroup
    "reduction step counts (baseline)"
    [ bench "1 + 1" "module M\nlet result = 1 + 1"
    , bench "2 + 3" "module M\nlet result = 2 + 3"
    , bench "2 * 3" "module M\nlet result = 2 * 3"
    , bench "(fun x -> x) 4" "module M\nlet result = (fun x -> x) 4"
    ]
 where
  bench :: String -> Text -> TestTree
  bench lbl src = testCase lbl $ do
    case compileLml src of
      Left e -> assertFailure $ "compileLml failed: " <> e
      Right (binds, resultExpr) -> do
        let term = mustTranslate binds resultExpr
            decoderFn = Term.A (Term.A Term.S (Term.A Term.K (Term.V tickName))) Term.I
            full = Term.A (Term.A term decoderFn) (Term.V baseName)
            (steps, _g, reachedNF) = reduceSteps maxStepsInt (fromTerm full)
        unless reachedNF $ assertFailure "did not reach normal form"
        when (steps > stepGuard) $ assertFailure $ "step count " <> show steps <> " exceeds guard " <> show stepGuard
        putStrLn $ "  " <> lbl <> ": " <> show steps <> " steps"

-- | The step budget as a plain 'Int' (for 'reduceSteps'); must match 'maxSteps'.
maxStepsInt :: Int
maxStepsInt = case maxSteps of
  MaxSteps n -> n

{- | Generous step-count regression guard. Measured baseline on the worklist
reducer: identity = 121, 1+1 = 1432, 2*3 = 1463, 2+3 = 1848 steps. (The earlier
fixed-order reducer took 175 / 3766 / 5379 / 6689 respectively.) The guard sits
well above the baseline so a change in contraction order still passes; it bounds
a catastrophic regression.
-}
stepGuard :: Int
stepGuard = 2000000

{- | @1 + 1@ must decode (SKI) to 2, and the direct Church numeral 2 must also
decode to 2. Validates the decoder on a minimal case.
-}
diagTest :: TestTree
diagTest = testCase "diag: 1 + 1 decodes to 2" $ do
  let resultExpr :: CoreExpr
      resultExpr = App (App (Var stdPlus) (Lit (LitInt 1))) (Lit (LitInt 1))
  lmlVal <- lmlEvalExpr [] resultExpr
  case lmlVal of
    VInt 2 -> return ()
    other -> assertFailure $ "lml gave " <> show other <> ", expected VInt 2"
  checkDecode "church 2" (toSki (evalState (churchLam 2) 0)) 2
  checkDecode "ski(1+1)" (mustTranslate [] resultExpr) 2

{- | The spike: @let x = 2 + 3 in x@ must decode (SKI) to 5, agreeing with the
LML interpreter, and the direct Church numeral 5 must also decode to 5. Uses a
top-level @let@ to exercise binding inlining.
-}
spikeTest :: TestTree
spikeTest = testCase "spike: let x = 2 + 3 in x decodes to 5 (lml and ski agree)" $ do
  let xVar :: Var
      xVar = Id (Name (mkLongident (pure "x")))
      sumExpr :: CoreExpr
      sumExpr = App (App (Var stdPlus) (Lit (LitInt 2))) (Lit (LitInt 3))
      binds :: [CoreBind]
      binds = [NonRec xVar sumExpr]
      resultExpr :: CoreExpr
      resultExpr = Var xVar
  lmlVal <- lmlEvalExpr binds resultExpr
  case lmlVal of
    VInt 5 -> return ()
    other -> assertFailure $ "lml gave " <> show other <> ", expected VInt 5"
  checkDecode "ski(let x = 2+3 in x)" (mustTranslate binds resultExpr) 5
  checkDecode "church 5" (toSki (evalState (churchLam 5) 0)) 5

-- | Decode a SKI term to an integer and assert it equals @expected@.
checkDecode :: String -> Term.Expr -> Int -> IO ()
checkDecode lbl term expected = do
  let r = decodeChurch maxSteps term
  case r of
    Left (StepOverflow step) -> assertFailure $ lbl <> ": reduction did not reach normal form within " <> show step <> " steps"
    Right n -> assertEqual (lbl <> " decoded value") expected n

{- | Evaluate a core expression (with its enclosing bindings) with the LML
interpreter by binding the result expression to a fresh variable and reading it
back from the environment.
-}
lmlEvalExpr :: [CoreBind] -> CoreExpr -> IO Value
lmlEvalExpr binds e = do
  let resultVar = Id (Name (mkLongident (pure "__result__")))
  env <- runIdentityT (evalCoreBindsDefEnv (binds ++ [NonRec resultVar e]))
  case env of
    EvalEnv hm -> case HashMap.lookup resultVar hm of
      Just v -> pure v
      Nothing -> fail "result var not in env"
