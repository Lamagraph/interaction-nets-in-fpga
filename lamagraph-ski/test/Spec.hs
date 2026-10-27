module Main (main) where

import Relude

import Common.Term qualified as Term
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Lazy qualified as HashMap
import Data.List (lookup)
import Data.Text qualified as T
import GraphRewriting (Graph)
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
      , runnerTests
      , equivalenceTests
       , skiEvalTests
       , boolTests
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
  equiv label term = testCase label $ do
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

-- | Evaluate a core program via 'skiEval' and assert it equals @expected@. The
-- expected value doubles as the decode shape (the decoder is type-directed).
assertSkiEval :: [CoreBind] -> CoreExpr -> Value' -> IO ()
assertSkiEval binds expr expected = case skiEval expected binds expr of
  Right v -> assertEqual "skiEval" expected v
  Left e -> assertFailure $ "skiEval failed: " <> show e

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

propertyTests :: TestTree
propertyTests =
  testGroup
    "differential property (minimal subset)"
    [ testProperty "lml == ski for all (program, input)" propDifferential
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
  bench label src = testCase label $ do
    case compileLml src of
      Left e -> assertFailure $ "compileLml failed: " <> e
      Right (binds, resultExpr) -> do
        let term = mustTranslate binds resultExpr
            decoderFn = Term.A (Term.A Term.S (Term.A Term.K (Term.V tickName))) Term.I
            full = Term.A (Term.A term decoderFn) (Term.V baseName)
            (steps, _g, reachedNF) = reduceSteps maxStepsInt (fromTerm full)
        unless reachedNF $ assertFailure "did not reach normal form"
        when (steps > stepGuard) $ assertFailure $ "step count " <> show steps <> " exceeds guard " <> show stepGuard
        putStrLn $ "  " <> label <> ": " <> show steps <> " steps"

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
checkDecode label term expected = do
  let r = decodeChurch maxSteps term
  case r of
    Left (StepOverflow step) -> assertFailure $ label <> ": reduction did not reach normal form within " <> show step <> " steps"
    Right n -> assertEqual (label <> " decoded value") expected n

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
