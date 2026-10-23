module Lamagraph.Compiler.Typechecker.InferTest (inferUnitTests) where

import Relude hiding (show)

import Prelude (show)
import Test.Tasty
import Test.Tasty.HUnit

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Types (FullName (..))
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Infer
import Lamagraph.Compiler.Typechecker.TcTypes

l_ :: a -> Located a
l_ = L generatedSrcSpan

patVar :: Text -> LmlPat LmlcMr
patVar name = LmlPatVar (FullName (Longident (pure name))) (l_ name)

constantExpr :: LmlExpr LmlcMr
constantExpr = LmlExprConstant noExtField (LmlInt noExtField 1)

constructExpr :: Longident -> LmlExpr LmlcMr
constructExpr ident = LmlExprConstruct noExtField (l_ ident) (Just (l_ constantExpr))

nonRecVald :: LmlPat LmlcMr -> LmlExpr LmlcMr -> LmlDecl LmlcMr
nonRecVald pat expr =
  ValD noExtField (l_ (LmlBindGroup noExtField NonRecursive (pure (l_ (LmlBind noExtField (l_ pat) (l_ expr))))))

recVald :: LmlPat LmlcMr -> LmlExpr LmlcMr -> LmlDecl LmlcMr
recVald pat expr =
  ValD noExtField (l_ (LmlBindGroup noExtField Recursive (pure (l_ (LmlBind noExtField (l_ pat) (l_ expr))))))

tyd :: LTyDecl LmlcMr -> LmlDecl LmlcMr
tyd = TyD noExtField . pure

modOf :: LmlDecl LmlcMr -> LmlModule LmlcMr
modOf decl =
  LmlModule
    { _lmlModExt = noExtField
    , _lmlModName = Just (l_ (Longident (pure "Ghost")))
    , _lmlModDecls = pure (l_ decl)
    }

expectError :: String -> LmlModule LmlcMr -> (TypecheckError -> Bool) -> TestTree
expectError testName module_ isError = testCase testName $ case inferDef module_ of
  Left err
    | isError err -> return ()
    | otherwise -> assertFailure (show err)
  Right _ -> assertFailure "expected a typecheck error"

inferUnitTests :: TestTree
inferUnitTests =
  testGroup
    "Infer unit tests"
    [ expectError
        "ConstructorDoesntExist on unknown constructor"
        (modOf (nonRecVald (patVar "x") (constructExpr (Longident (pure "NoSuchConstr")))))
        (\case
          ConstructorDoesntExist (Name (Longident ("NoSuchConstr" :| []))) -> True
          _ -> False
        )
    , expectError
        "NonVariableInLetRec on constant pattern in rec group"
        (modOf (recVald (LmlPatConstant noExtField (LmlInt noExtField 5)) constantExpr))
        (\case
          NonVariableInLetRec -> True
          _ -> False
        )
    , let badAliasParam = l_ (LmlTyConstr noExtField (l_ (Longident (pure "int"))) (pure (l_ (LmlTyVar noExtField (l_ "a")))))
          badAlias = l_ (AliasDecl noExtField (l_ "badparam") (pure badAliasParam) (l_ (LmlTyVar noExtField (l_ "a"))))
      in expectError
        "InvalidTypeParameter on non-variable type parameter"
        (modOf (tyd badAlias))
        (\case
          InvalidTypeParameter -> True
          _ -> False
        )
    ]
