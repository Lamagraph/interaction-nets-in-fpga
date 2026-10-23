module Lamagraph.Compiler.Eval.EvalTest (evalUnitTests) where

import Relude hiding (show)

import Control.Exception (try)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude (show)

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Eval
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

-- | A bare (unqualified) local 'Var'.
vname :: Text -> Var
vname n = Id (Name (Longident (pure n)))

-- | A 'Var' in the @#std@ module, as bound by 'defEvalEnv'.
vstd :: Text -> Var
vstd n = Id (Name (Longident (stdPrefix :| [n])))

-- | A 'DataCon' (a 'Name') in the @#std@ module.
dstd :: Text -> DataCon
dstd n = Name (Longident (stdPrefix :| [n]))

-- | Unwraps an 'Eval' ('IdentityT IO') computation to a plain 'IO' action.
runEval :: Eval a -> IO a
runEval = runIdentityT

{- | Runs 'evalCoreBinds' under 'Eval' and deep-forces the resulting env (via
'show') so that a deeply lazy exception surfaces inside the 'try' instead
of escaping later as a poisoned thunk.
-}
evalBindsOrFail :: [CoreBind] -> IO (Either EvalException EvalEnv)
evalBindsOrFail binds =
  try
    ( do
        env <- runEval (evalCoreBinds defEvalEnv binds)
        forceShow env
        return env
    )
 where
  -- Fully forces 'x' (via its 'Show' rendering); 'n `seq`' ensures the
  -- evaluation actually happens during the IO action.
  forceShow :: (Show a) => a -> IO ()
  forceShow x =
    let n = length (show x)
     in n `seq` return ()

{- | Evaluates the binds and asserts the result is an 'EvalException' matched
by 'isError'. Also forces the 'Show' of the exception.
-}
expectEvalError :: String -> [CoreBind] -> (EvalException -> Bool) -> TestTree
expectEvalError testName binds isError = testCase testName $ do
  result <- evalBindsOrFail binds
  case result of
    Left err ->
      let shown = show err
       in if isError err
            then return ()
            else assertFailure shown
    Right _ -> assertFailure "expected an eval exception"

{- | Asserts that 'show'ing the value yields a non-empty string (fully
forcing the rendering to enter the derived 'Show' method).
-}
assertShowNonEmpty :: (Show a) => String -> a -> Assertion
assertShowNonEmpty label x =
  let s = show x
   in when (null s) (assertFailure (label <> " rendered empty"))

valueShowTests :: TestTree
valueShowTests =
  testGroup
    "Eval Show instances"
    [ testCase "Value ctors" $ do
        assertShowNonEmpty "VInt" (VInt 1 :: Value)
        assertShowNonEmpty "VChar" (VChar 'a')
        assertShowNonEmpty "VString" (VString "hi")
        assertShowNonEmpty "VClosure" (VClosure (vname "x") (Lit (LitInt 1)) defEvalEnv)
        assertShowNonEmpty "VUnaryPrim" (VUnaryPrim UPPrintInt)
        assertShowNonEmpty "VBinaryPrim1" (VBinaryPrim1 BPGreater)
        assertShowNonEmpty "VBinaryPrim2" (VBinaryPrim2 BPLessEq (VInt 1))
        assertShowNonEmpty "VTuple" (VTuple (VInt 1) (pure (VInt 2)))
        assertShowNonEmpty "VAdt" (VAdt (dstd "None") [VInt 1])
    , testCase "UnaryPrim / BinaryPrim" $ do
        assertShowNonEmpty "UPMinus" (UPMinus :: UnaryPrim)
        assertShowNonEmpty "BPPlus" (BPPlus :: BinaryPrim)
        assertShowNonEmpty "BPTimes" BPTimes
    , testCase "EvalEnv" $ assertShowNonEmpty "defEvalEnv" defEvalEnv
    , testCase "EvalException" $ do
        assertShowNonEmpty "EUnboundVariable" (EUnboundVariable (vname "x") :: EvalException)
        assertShowNonEmpty "EInvalidUnaryApply" (EInvalidUnaryApply UPMinus (VInt 1))
        assertShowNonEmpty "EInvalidBinaryApply" (EInvalidBinaryApply BPPlus (VInt 1) (VInt 2))
        assertShowNonEmpty "EInvalidValueApply" (EInvalidValueApply (VInt 1) (VChar 'a'))
        assertShowNonEmpty "EMatchFailed" EMatchFailed
        assertShowNonEmpty "ENonLambdaUnderLetRec" ENonLambdaUnderLetRec
    ]

evalUnitTests :: TestTree
evalUnitTests =
  testGroup
    "Eval unit tests"
    [ valueShowTests
    , expectEvalError
        "EUnboundVariable on unbound var"
        [NonRec (vname "x") (Var (vname "ghost"))]
        ( \case
            EUnboundVariable v -> v == vname "ghost"
            _ -> False
        )
    , expectEvalError
        "EInvalidUnaryApply on UPMinus over char"
        [NonRec (vname "x") (App (Var (vstd "~-")) (Lit (LitChar 'a')))]
        ( \case
            EInvalidUnaryApply UPMinus (VChar 'a') -> True
            _ -> False
        )
    , expectEvalError
        "EInvalidBinaryApply on BPPlus over chars"
        [ NonRec
            (vname "x")
            (App (App (Var (vstd "+")) (Lit (LitChar 'a'))) (Lit (LitChar 'b')))
        ]
        ( \case
            EInvalidBinaryApply BPPlus (VChar 'a') (VChar 'b') -> True
            _ -> False
        )
    , expectEvalError
        "EInvalidValueApply on applying an int"
        [NonRec (vname "x") (App (Lit (LitInt 5)) (Lit (LitInt 1)))]
        ( \case
            EInvalidValueApply (VInt 5) (VInt 1) -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on int literal mismatch"
        [NonRec (vname "x") (Match (Lit (LitInt 5)) (vname "s") (pure (LitAlt (LitInt 1), [], Lit (LitInt 1))))]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on char literal mismatch"
        [NonRec (vname "x") (Match (Lit (LitChar 'a')) (vname "s") (pure (LitAlt (LitChar 'b'), [], Lit (LitInt 1))))]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on string literal mismatch"
        [NonRec (vname "x") (Match (Lit (LitString "x")) (vname "s") (pure (LitAlt (LitString "y"), [], Lit (LitInt 1))))]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on data constructor mismatch"
        [ NonRec
            (vname "x")
            (Match (App (Var (vstd "Some")) (Lit (LitInt 1))) (vname "s") (pure (DataAlt (dstd "None"), [], Lit (LitInt 1))))
        ]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on tuple arity mismatch"
        [ NonRec
            (vname "x")
            ( Match
                (Tuple (Lit (LitInt 1)) (pure (Lit (LitInt 2))))
                (vname "s")
                (pure (TupleAlt, [vname "a", vname "b", vname "c"], Lit (LitInt 1)))
            )
        ]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "EMatchFailed on value/alt shape mismatch"
        [NonRec (vname "x") (Match (Lit (LitInt 1)) (vname "s") (pure (DataAlt (dstd "None"), [], Lit (LitInt 1))))]
        ( \case
            EMatchFailed -> True
            _ -> False
        )
    , expectEvalError
        "ENonLambdaUnderLetRec on non-lambda rec bind"
        [Rec (pure (vname "f", Lit (LitInt 5)))]
        ( \case
            ENonLambdaUnderLetRec -> True
            _ -> False
        )
    ]
