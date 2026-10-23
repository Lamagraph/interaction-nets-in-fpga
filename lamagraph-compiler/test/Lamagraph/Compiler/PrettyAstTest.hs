module Lamagraph.Compiler.PrettyAstTest (prettyAstTests) where

import Relude

import Data.HashMap.Strict qualified as HashMap
import Prettyprinter (Pretty, pretty)
import Test.Tasty
import Test.Tasty.HUnit

import Lamagraph.Compiler.GoldenCommon (renderPretty)
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.PrettyAst ()
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

-- Dummy values used to drive the 'Pretty' instances.
dummyName :: Name
dummyName = Name (Longident (pure "x"))

dummyFullName :: FullName
dummyFullName = FullName (Longident ("mod" :| ["x"]))

dummyLongident :: Longident
dummyLongident = Longident (pure "x")

dummyTy :: Ty
dummyTy = TConstr dummyName [TVar dummyName]

dummyTyScheme :: TyScheme
dummyTyScheme = Forall [dummyName] dummyTy

dummyTyEnv :: TyEnv
dummyTyEnv = TyEnv (HashMap.fromList [(dummyName, dummyTyScheme)])

dummyRealSrcSpan :: RealSrcSpan
dummyRealSrcSpan = mkRealSrcSpan (mkRealSrcLoc "f.lml" 1 5) (mkRealSrcLoc "f.lml" 3 9)

{- | Renders the value and asserts the result is non-empty (entering the
'Pretty' method).
-}
assertRenders :: (Pretty a) => String -> a -> TestTree
assertRenders label x = testCase label $ do
  let s = renderPretty (pretty x)
  when (s == mempty) (assertFailure (label <> " rendered empty"))

typecheckErrorTests :: TestTree
typecheckErrorTests =
  testGroup
    "TypecheckError"
    [ assertRenders "UnboundVariable" (UnboundVariable dummyName :: TypecheckError)
    , assertRenders "ConstructorDoesntExist" (ConstructorDoesntExist dummyName)
    , assertRenders "OccursCheck" (OccursCheck dummyName dummyTy)
    , assertRenders "CantUnify" (CantUnify dummyTy (TVar dummyName))
    , assertRenders "NonVariableInLetRec" NonVariableInLetRec
    , assertRenders "VariableClashInPattern" (VariableClashInPattern dummyName)
    , assertRenders "VarMustOccurOnBothSidesOfOrPattern" (VarMustOccurOnBothSidesOfOrPattern dummyName)
    , assertRenders "UndefinedTypeVariable" (UndefinedTypeVariable dummyName)
    , assertRenders "DuplicateConstructor" (DuplicateConstructor dummyName)
    , assertRenders "DuplicateTypeParameter" (DuplicateTypeParameter dummyName)
    , assertRenders "UnboundTypeConstructor" (UnboundTypeConstructor dummyName)
    , assertRenders "ArityMismatch" (ArityMismatch dummyName 2 3)
    , assertRenders "InvalidTypeParameter" InvalidTypeParameter
    ]

moduleResolverErrorTests :: TestTree
moduleResolverErrorTests =
  testGroup
    "ModuleResolverError"
    [ assertRenders "NameNotFound" (NameNotFound dummyLongident :: ModuleResolverError)
    , assertRenders "ModuleNotFound" (ModuleNotFound dummyLongident)
    , assertRenders "ConstructorNotFound" (ConstructorNotFound dummyLongident)
    ]

utilityInstanceTests :: TestTree
utilityInstanceTests =
  testGroup
    "Utility / typechecker Pretty instances"
    [ assertRenders "RealSrcSpan" dummyRealSrcSpan
    , assertRenders "UnhelpfulGenerated" (UnhelpfulGenerated :: UnhelpfulSpanReason)
    , assertRenders "UnhelpfulOther" (UnhelpfulOther "bad")
    , assertRenders "SrcSpan (real)" (RealSrcSpan dummyRealSrcSpan :: SrcSpan)
    , assertRenders "SrcSpan (generated)" (UnhelpfulSpan UnhelpfulGenerated)
    , assertRenders "SrcSpan (other)" (UnhelpfulSpan (UnhelpfulOther "bad"))
    , assertRenders "Located a" (L generatedSrcSpan (5 :: Int))
    , assertRenders "Located Text" (L generatedSrcSpan ("hello" :: Text))
    , assertRenders "Recursive" (Recursive :: RecFlag)
    , assertRenders "NonRecursive" NonRecursive
    , assertRenders "TyEnv" dummyTyEnv
    , assertRenders "(Name, TyScheme)" (dummyName, dummyTyScheme)
    , assertRenders "TyScheme (no forall)" (Forall [] dummyTy :: TyScheme)
    , assertRenders "TyScheme (forall)" dummyTyScheme
    , assertRenders "FullName" dummyFullName
    , assertRenders "Name" dummyName
    , assertRenders "Longident" dummyLongident
    , assertRenders "Ty (TVar)" (TVar dummyName :: Ty)
    , assertRenders "Ty (TArrow)" (TArrow (TVar dummyName) dummyTy)
    , assertRenders "Ty (TConstr 0)" (TConstr dummyName [] :: Ty)
    , assertRenders "Ty (TConstr 1)" (TConstr dummyName [dummyTy])
    , assertRenders "Ty (TConstr n)" (TConstr dummyName [dummyTy, dummyTy])
    , assertRenders "Ty (TTuple)" (TTuple (TVar dummyName) (pure dummyTy))
    , assertRenders "Either (Left)" (Left (UnboundVariable dummyName) :: Either TypecheckError Int)
    , assertRenders "Either (Right)" (Right (5 :: Int) :: Either TypecheckError Int)
    ]

prettyAstTests :: TestTree
prettyAstTests =
  testGroup
    "PrettyAst"
    [typecheckErrorTests, moduleResolverErrorTests, utilityInstanceTests]
