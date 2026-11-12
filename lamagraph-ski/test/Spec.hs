module Main (main) where

import Relude

import Common.Term qualified as Term
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Lazy qualified as HashMap
import Data.List (lookup)
import Data.Set qualified as Set
import Data.Text qualified as T
import GraphRewriting (Graph, evalGraph, readNode, readNodeList)
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import INet.Graph (SKI (..), fromTerm)
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
import Lamagraph.Ski.YaleDecode (rootConstantNameYale, skiEvalYale)
import Lamagraph.Ski.YaleNet (YaleNode (YaleApp), buildYaleNet)
import Lamagraph.Ski.YaleReduce (reduceStepsYale, reduceStepsYaleTags, reduceStepsYaleTrace)

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
      , yaleEvalTests
      , debugYale3
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
('LSelfRef', 'LRec', 'LMutRec'): a self-reference contributes no free
variable, group names are bound in a mutual group, and substitution respects
the binders (a bound name is not substituted; a group name is protected).
-}
recIrTests :: TestTree
recIrTests =
  testGroup
    "recursion IR (fv/subst)"
    [ testCase "fv LSelfRef = empty" $ assertEqual "fv LSelfRef" Set.empty (fv LSelfRef)
    , testCase "fv LRec: binder is bound, self-ref is not free" $
        assertEqual
          "fv (LRec (LLam f (self x)))"
          (Set.fromList ["x"])
          (fv (LRec (LLam "f" (LApp LSelfRef (LVar "x")))))
    , testCase "fv LMutRec: group names are bound" $
        assertEqual
          "fv (LMutRec [(even, ...), (odd, ...)] (even z))"
          (Set.fromList ["z"])
          (fv (LMutRec mutualGroups (LApp (LVar "even") (LVar "z"))))
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
    , testCase "subst: a group name is protected in an LMutRec" $
        assertEqual
          "subst even w (LMutRec groups (even z))"
          (LMutRec mutualGroups (LApp (LVar "even") (LVar "z")))
          (subst "even" (LVar "w") (LMutRec mutualGroups (LApp (LVar "even") (LVar "z"))))
    , testCase "subst: an external name recurses into an LMutRec" $
        assertEqual
          "subst z w (LMutRec groups z)"
          (LMutRec mutualGroups (LVar "w"))
          (subst "z" (LVar "w") (LMutRec mutualGroups (LVar "z")))
    ]
 where
  -- \| A mutual-recursion group: @even@ and @odd@ call each other.
  mutualGroups :: [(String, Lam)]
  mutualGroups =
    [ ("even", LLam "n" (LApp (LVar "odd") (LVar "n")))
    , ("odd", LLam "n" (LApp (LVar "even") (LVar "n")))
    ]

{- | Tests for 'translateLam': 'let rec' produces an 'LRec' with an 'LSelfRef'
self-reference, mutual 'let rec' produces an 'LMutRec', a non-recursive program
carries no recursion markers, and 'translateCore' rejects recursive bindings
(the SKI backend is non-recursive-only).
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
    , testCase "mutual let rec -> LMutRec" $
        case compileLml
          "module M\nlet rec even n = if n < 1 then true else odd (n - 1) and odd n = if n < 1 then false else even (n - 1)\nlet result = even 4" of
          Left e -> assertFailure $ "compileLml failed: " <> e
          Right (binds, resultExpr) -> case translateLam binds resultExpr of
            Left e -> assertFailure $ "translateLam failed: " <> show e
            Right lam -> do
              assertBool "top level is LMutRec" (isLMutRec lam)
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

-- | Whether a lambda term is an 'LMutRec' at the top level.
isLMutRec :: Lam -> Bool
isLMutRec (LMutRec _ _) = True
isLMutRec _ = False

{- | Whether a lambda term contains a recursion marker ('LRec', 'LMutRec', or
'LSelfRef') anywhere.
-}
containsRecMarker :: Lam -> Bool
containsRecMarker = \case
  LRec _ -> True
  LMutRec _ _ -> True
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
  LMutRec groups body -> any (containsLSelfRef . snd) groups || containsLSelfRef body

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
decodeTicks :: Graph SKI -> Int
decodeTicks = countLogical isTick
 where
  isTick :: SKI -> Bool
  isTick = \case
    V{name = n} -> n == tickName
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

-- | Count the 'YaleApp' nodes in a reduced YALE net (temporary debug helper).
countAppsYale :: Graph YaleNode -> Int
countAppsYale = evalGraph go
 where
  go = do
    ns <- readNodeList
    mapM readNode ns
      <&> length
      . filter (\case YaleApp{} -> True; _ -> False)

-- | Dump all nodes of a YALE net (temporary debug helper).
dumpYaleNet :: Graph YaleNode -> IO ()
dumpYaleNet g =
  let m = do
        ns <- readNodeList
        mapM readNode ns
   in mapM_ (putStrLn . ("    node: " ++) . show) (evalGraph m g)

-- * Temporary debug 3 (S4-followup)
debugYale3 :: TestTree
debugYale3 =
  testGroup
    "yale-debug3"
    [ testCase "probe terms" $ do
        let church 0 = LLam "f" (LLam "x" (LVar "x"))
            church 1 = LLam "f" (LLam "x" (LApp (LVar "f") (LVar "x")))
            church 2 = LLam "f" (LLam "x" (LApp (LVar "f") (LApp (LVar "f") (LVar "x"))))
            church 3 = LLam "f" (LLam "x" (LApp (LVar "f") (LApp (LVar "f") (LApp (LVar "f") (LVar "x")))))
            church _ = error "church: unsupported"
        let probe label term = do
              putStrLn $ "  [probe] " <> label
              let full = LApp (LApp term (LVar "__tickT__")) (LVar "__tickF__")
              let g = buildYaleNet full
              let (steps, g2, nf) = reduceStepsYale 10000 g
              let root = rootConstantNameYale g2
              putStrLn $ "  " <> label <> ": steps=" <> show steps <> " nf=" <> show nf <> " root=" <> show root
        -- (\x. \y. x) 1 2  => 1
        let t1 = LApp (LApp (LLam "x" (LLam "y" (LVar "x"))) (church 1)) (church 2)
        -- (\x. \y. y) 1 2  => 2
        let t2 = LApp (LApp (LLam "x" (LLam "y" (LVar "y"))) (church 1)) (church 2)
        -- (\x. x x) (\y. y)  => (\y. y)  [duplication]
        let t3 = LApp (LLam "x" (LApp (LVar "x") (LVar "x"))) (LLam "y" (LVar "y"))
        -- (\f. \x. f (f x)) (\x. x)  => (\x. x)  [duplication of f]
        let t4 = LApp (LLam "f" (LLam "x" (LApp (LVar "f") (LApp (LVar "f") (LVar "x"))))) (LLam "x" (LVar "x"))
        -- Trace K12 step by step to find the dangling edge.
        let fullK12 = LApp (LApp t1 (LVar "__tickT__")) (LVar "__tickF__")
        let gK12 = buildYaleNet fullK12
        (sK12, _, nfK12) <- reduceStepsYaleTrace 30 gK12
        putStrLn $ "  K12 trace done: steps=" <> show sK12 <> " nf=" <> show nfK12
        probe "K12 -> 1" t1
        probe "K22 -> 2" t2
        -- Tag trace for twice-id to see if a Copy interaction ever fires.
        let fullT4 = LApp (LApp t4 (LVar "__tickT__")) (LVar "__tickF__")
        let gT4 = buildYaleNet fullT4
        (sT4, _, nfT4) <- reduceStepsYaleTags 40 gT4
        putStrLn $ "  twice-id tag-trace done: steps=" <> show sT4 <> " nf=" <> show nfT4
        probe "twice-id" t4
        -- Tag trace for dup-id to diagnose non-termination.
        let fullT3 = LApp (LApp t3 (LVar "__tickT__")) (LVar "__tickF__")
        let gT3 = buildYaleNet fullT3
        (sT3, _, nfT3) <- reduceStepsYaleTags 40 gT3
        putStrLn $ "  dup-id tag-trace done: steps=" <> show sT3 <> " nf=" <> show nfT3
        probe "dup-id" t3
        -- Minimal failing case: duplicate a lambda whose body is an application.
        -- (\x. x x) (\y. y y)  =>  (\y. y y)  [duplication of a lambda with an app body]
        let t5 = LApp (LLam "x" (LApp (LVar "x") (LVar "x"))) (LLam "y" (LApp (LVar "y") (LVar "y")))
        probe "dup-app (expect id)" t5
        -- Trace the minimal failing case step by step.
        let full5 = LApp (LApp t5 (LVar "__tickT__")) (LVar "__tickF__")
        let g5 = buildYaleNet full5
        (s5, _, nf5) <- reduceStepsYaleTrace 30 g5
        putStrLn $ "  dup-app trace done: steps=" <> show s5 <> " nf=" <> show nf5
        -- TEMPORARY: trace dup-id step by step with net dumps
        let full3 = LApp (LApp t3 (LVar "__tickT__")) (LVar "__tickF__")
        let g3 = buildYaleNet full3
        (s3, _, nf3) <- reduceStepsYaleTrace 25 g3
        putStrLn $ "  dup-id trace done: steps=" <> show s3 <> " nf=" <> show nf3
        -- Simple boolean probes (expect tickT / tickF)
        let trueL = LLam "a" (LLam "b" (LVar "a"))
        let falseL = LLam "a" (LLam "b" (LVar "b"))
        let isZeroL = LLam "n" (LApp (LApp (LVar "n") (LLam "x" falseL)) trueL)
        probe "id true (T)" (LApp (LLam "x" (LVar "x")) trueL)
        probe "id false (F)" (LApp (LLam "x" (LVar "x")) falseL)
        probe "K T F (T)" (LApp (LApp (LLam "x" (LLam "y" (LVar "x"))) trueL) falseL)
        probe "K' T F (F)" (LApp (LApp (LLam "x" (LLam "y" (LVar "y"))) trueL) falseL)
        probe "isZero 0 (T)" (LApp isZeroL (church 0))
        probe "isZero 1 (F)" (LApp isZeroL (church 1))
        probe "isZero 2 (F)" (LApp isZeroL (church 2))
        -- pred 2 should be 1, so isZero (pred 2) should be false (tickF)
        let predL =
              LLam
                "n"
                ( LLam
                    "f"
                    ( LLam
                        "x"
                        ( LApp
                            ( LApp
                                (LApp (LVar "n") (LLam "g" (LLam "h" (LApp (LVar "h") (LApp (LVar "g") (LVar "f"))))))
                                (LLam "u1" (LVar "x"))
                            )
                            (LLam "u2" (LVar "u2"))
                        )
                    )
                )
        probe "isZero(pred 1) (T)" (LApp isZeroL (LApp predL (church 1)))
        probe "isZero(pred 2) (F)" (LApp isZeroL (LApp predL (church 2)))
        probe "isZero(pred 3) (F)" (LApp isZeroL (LApp predL (church 3)))
        -- check 1 < 2 and 2 < 1
        let src12 = "module M\nlet result = 1 < 2"
        let src21 = "module M\nlet result = 2 < 1"
        case compileLml src12 of
          Left e -> assertFailure $ "compile 1<2: " <> e
          Right (b12, e12) ->
            case translateLam b12 e12 of
              Left e -> assertFailure $ "translateLam 1<2: " <> show e
              Right lam12 -> do
                probe "1<2 (expect tickT)" lam12
                let full12 = LApp (LApp lam12 (LVar "__tickT__")) (LVar "__tickF__")
                let g12 = buildYaleNet full12
                (s12, g12b, nf12) <- reduceStepsYaleTrace 200 g12
                putStrLn $ "  1<2 trace done: steps=" <> show s12 <> " nf=" <> show nf12
                let mDump12 = do
                      ns <- readNodeList
                      mapM readNode ns
                    dump12 = evalGraph mDump12 g12b
                mapM_ (putStrLn . ("    1<2 node: " ++) . show) dump12
        case compileLml src21 of
          Left e -> assertFailure $ "compile 2<1: " <> e
          Right (b21, e21) ->
            case translateLam b21 e21 of
              Left e -> assertFailure $ "translateLam 2<1: " <> show e
              Right lam21 -> do
                probe "2<1 (expect tickF)" lam21
                -- dump the reduced net structure
                let full21 = LApp (LApp lam21 (LVar "__tickT__")) (LVar "__tickF__")
                let g21 = buildYaleNet full21
                let (s21, g21b, nf21) = reduceStepsYale 10000 g21
                putStrLn $ "  2<1 net: steps=" <> show s21 <> " nf=" <> show nf21
                let mDump = do
                      ns <- readNodeList
                      mapM readNode ns
                    dump = evalGraph mDump g21b
                mapM_ (putStrLn . ("    node: " ++) . show) dump
    , testCase "pred differential (YALE church decode)" $ do
        let church 0 = LLam "f" (LLam "x" (LVar "x"))
            church 1 = LLam "f" (LLam "x" (LApp (LVar "f") (LVar "x")))
            church 2 = LLam "f" (LLam "x" (LApp (LVar "f") (LApp (LVar "f") (LVar "x"))))
            church 3 = LLam "f" (LLam "x" (LApp (LVar "f") (LApp (LVar "f") (LApp (LVar "f") (LVar "x")))))
            church _ = error "church: unsupported"
        let predL =
              LLam
                "n"
                ( LLam
                    "f"
                    ( LLam
                        "x"
                        ( LApp
                            ( LApp
                                (LApp (LVar "n") (LLam "g" (LLam "h" (LApp (LVar "h") (LApp (LVar "g") (LVar "f"))))))
                                (LLam "u1" (LVar "x"))
                            )
                            (LLam "u2" (LVar "u2"))
                        )
                    )
                )
        let decodeChurch label term = do
              let full = LApp (LApp term (LVar "__tick__")) (LVar "__base__")
              let g = buildYaleNet full
              let (steps, g2, nf) = reduceStepsYale 20000 g
              let apps = countAppsYale g2
              putStrLn $ "  " <> label <> ": steps=" <> show steps <> " nf=" <> show nf <> " apps=" <> show apps
        decodeChurch "pred 0 (expect 0)" (LApp predL (church 0))
        decodeChurch "pred 1 (expect 0)" (LApp predL (church 1))
        decodeChurch "pred 2 (expect 1)" (LApp predL (church 2))
        decodeChurch "pred 3 (expect 2)" (LApp predL (church 3))
        -- Lightweight tag trace for pred 2 to detect a loop.
        let fullP2 = LApp (LApp (LApp predL (church 2)) (LVar "__tick__")) (LVar "__base__")
        let gP2 = buildYaleNet fullP2
        (sP2, _, nfP2) <- reduceStepsYaleTags 400 gP2
        putStrLn $ "  pred 2 tag-trace done: steps=" <> show sP2 <> " nf=" <> show nfP2
        -- Dump the net at step 12 (when the c x @ loop starts) to find the cycle.
        let (_, gP2s12, _) = reduceStepsYale 12 gP2
        putStrLn "  --- pred 2 net at step 12 ---"
        dumpYaleNet gP2s12
        let (_, gP2s13, _) = reduceStepsYale 13 gP2
        putStrLn "  --- pred 2 net at step 13 ---"
        dumpYaleNet gP2s13
        let (_, gP2s14, _) = reduceStepsYale 14 gP2
        putStrLn "  --- pred 2 net at step 14 ---"
        dumpYaleNet gP2s14
        let (_, gP2s15, _) = reduceStepsYale 15 gP2
        putStrLn "  --- pred 2 net at step 15 ---"
        dumpYaleNet gP2s15
        let (_, gP2s16, _) = reduceStepsYale 16 gP2
        putStrLn "  --- pred 2 net at step 16 ---"
        dumpYaleNet gP2s16
        -- Coarse-grained dumps to track the divergence.
        let dumpAt n = do
              let (_, gN, _) = reduceStepsYale n gP2
              putStrLn $ "  --- pred 2 net at step " <> show n <> " ---"
              dumpYaleNet gN
        mapM_ dumpAt [16, 17, 18, 19, 20, 21, 22, 23, 24]
        -- Dump the reduced net for pred 2 to inspect the interference.
        let full2 = LApp (LApp (LApp predL (church 2)) (LVar "__tick__")) (LVar "__base__")
        let g2 = buildYaleNet full2
        let (_, g2c, _) = reduceStepsYale 0 g2
        putStrLn "  --- pred 2 INITIAL net (post-compileBinders) ---"
        dumpYaleNet g2c
        let (_, g2r, _) = reduceStepsYale 20000 g2
        putStrLn "  --- pred 2 reduced net ---"
        dumpYaleNet g2r
    ]

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
    -- TODO(#91): DEFERRED (temporarily off, per user directive to focus on
    -- YALE recursion). The YALE reducer reduces '2 < 1' to a normal form
    -- whose root edge dangles (an App node whose func port points to a
    -- fresh edge with no term), so the decoder reports StepOverflow 0.
    -- Root cause: 'predLam' is the standard NON-saturating Church pred
    -- (pred 0 = identity, not 0), so 'sub 1 2' yields identity instead of
    -- 0 and the comparison walks the wrong branch. Fixing 'predLam' to be
    -- saturating exposes a secondary YALE-reducer bug (dangling edge on the
    -- more complex term shape), which must be fixed together. Re-enable
    -- this test (and add '1 < 2') once both are fixed, then run the full S1
    -- corpus through 'skiEvalYale' to close the S5 gate.
    -- , testCase "2 < 1" $ assertSkiEvalYale [] (binOp stdLess 2 1) (RBool False)
    ]

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
 where
  scrut = Id (Name (mkLongident (pure "t#a")))
  boolAlt :: Var -> CoreExpr -> CoreMatchAlt
  boolAlt d e = (DataAlt (nameOfVar d), [], e)
   where
    nameOfVar (Id n) = n

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
