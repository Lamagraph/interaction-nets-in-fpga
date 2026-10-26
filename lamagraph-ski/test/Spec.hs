module Main (main) where

import Relude

import Common.Term qualified as Term
import Data.HashMap.Lazy qualified as HashMap
import Test.Tasty
import Test.Tasty.HUnit

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Eval
import Lamagraph.Compiler.Syntax.Longident (mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski

{- | Step budget for reducing a decoded term to normal form. Generous because
the naive SKI translation is verbose and the fixed-order reducer is slow.
-}
maxSteps :: Int
maxSteps = 500000

main :: IO ()
main = defaultMain $ testGroup "lamagraph-ski" [valueTests, churchTests, translateTests, diagTest, spikeTest]

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
        [ testCase "int literal" $ assertClosed (translateCore [] (Lit (LitInt 5)))
        , testCase "plus" $ assertClosed (translateCore [] (binOp stdPlus 2 3))
        , testCase "times" $ assertClosed (translateCore [] (binOp stdTimes 2 3))
        , testCase "minus" $ assertClosed (translateCore [] (binOp stdMinus 5 2))
        , testCase "nested let" $
            let xVar = Id (Name (mkLongident (pure "x")))
             in assertClosed (translateCore [NonRec xVar (binOp stdPlus 2 3)] (Var xVar))
        , testCase "lambda + application" $
            let xVar = Id (Name (mkLongident (pure "x")))
                lamApp = App (Lam xVar (App (App (Var stdPlus) (Var xVar)) (Lit (LitInt 1)))) (Lit (LitInt 4))
             in assertClosed (translateCore [] lamApp)
        ]
    , testCase "end-to-end: 2 * 3 = 6" $ checkDecode "translate(2*3)" (translateCore [] (binOp stdTimes 2 3)) 6
    ]

-- | Build a core binary-operator application @(op a b)@.
binOp :: Var -> Int -> Int -> CoreExpr
binOp op a b = App (App (Var op) (Lit (LitInt a))) (Lit (LitInt b))

-- | Assert that a SKI term is closed (has no free @V@ nodes).
assertClosed :: Term.Expr -> Assertion
assertClosed term = assertEqual "free V nodes" 0 (countV term)

-- | Count the @V@ (variable) nodes in a SKI term.
countV :: Term.Expr -> Int
countV = \case
  Term.V _ -> 1
  Term.A f x -> countV f + countV x
  _ -> 0

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
  checkDecode "ski(1+1)" (translateCore [] resultExpr) 2

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
  checkDecode "ski(let x = 2+3 in x)" (translateCore binds resultExpr) 5
  checkDecode "church 5" (toSki (evalState (churchLam 5) 0)) 5

-- | Decode a SKI term to an integer and assert it equals @expected@.
checkDecode :: String -> Term.Expr -> Int -> IO ()
checkDecode label term expected = do
  let r = decodeChurch maxSteps term
  case r of
    Left step -> assertFailure $ label <> ": reduction did not reach normal form within " <> show step <> " steps"
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
