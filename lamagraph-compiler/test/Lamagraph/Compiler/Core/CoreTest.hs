module Lamagraph.Compiler.Core.CoreTest (coreUnitTests) where

import Relude hiding (show)

import Data.Hashable (hash)
import Data.Text qualified as T
import Prelude (show)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Typechecker.TcTypes

coreShowGoldenTestsDir :: FilePath
coreShowGoldenTestsDir = baseGoldenTestsDir </> "core" </> "show"

name_ :: [Text] -> Name
name_ = Name . mkLongident . go
 where
  go :: [Text] -> NonEmpty Text
  go (x : xs) = x :| xs
  go [] = "anon" :| []

var_ :: Name -> Var
var_ = Id

litInt :: Literal
litInt = LitInt 42

litChar :: Literal
litChar = LitChar 'a'

litString :: Literal
litString = LitString (T.pack "ab")

someVar :: Var
someVar = var_ (name_ ["x"])

otherVar :: Var
otherVar = var_ (name_ ["y"])

someDataCon :: DataCon
someDataCon = name_ ["#std", "Some"]

exprTree :: CoreExpr
exprTree =
  App
    (Lam someVar (App (Var someVar) (Lit litInt)))
    (Tuple (Lit litChar) (Lit litString :| []))

matchExpr :: CoreExpr
matchExpr =
  Match
    (Var someVar)
    otherVar
    ( (DataAlt someDataCon, [otherVar], Var otherVar)
      :| [ (LitAlt litInt, [], Lit litInt)
            , (LitAlt litChar, [], Lit litChar)
            , (LitAlt litString, [], Lit litString)
            , (TupleAlt, [someVar, otherVar], Tuple (Lit litInt) (Lit litInt :| []))
            , (DEFAULT, [], Lit litInt)
        ]
    )

letExpr :: CoreExpr
letExpr = Let (NonRec someVar (Var otherVar)) (Lit litInt)

nonRecBind :: CoreBind
nonRecBind = NonRec someVar (App (Var someVar) (Lit litInt))

recBind :: CoreBind
recBind =
  Rec
    ( (someVar, Lam someVar (Var someVar))
      :| [(otherVar, Lam otherVar (App (Var otherVar) (Lit litInt)))]
    )

showGoldens :: TestTree
showGoldens =
  testGroup
    "Core show goldens"
    [ goldenVsString name (addExtension (coreShowGoldenTestsDir </> name) ".show") (pure (encodeUtf8 value))
    | (name, value) <- goldenCases
    ]
 where
  goldenCases :: [(TestName, String)]
  goldenCases =
    [ ("literal_int", show litInt)
      , ("literal_char", show litChar)
      , ("literal_string", show litString)
      , ("var", show someVar)
      , ("expr_tree", show exprTree)
      , ("match", show matchExpr)
      , ("let", show letExpr)
      , ("bind_nonrec", show nonRecBind)
      , ("bind_rec", show recBind)
      ]

derivedInstanceTests :: TestTree
derivedInstanceTests =
  testGroup
    "Core derived instances"
    [ testCase "Literal show is stable" $ do
        show (show litInt) @?= show (show litInt)
        show (show litChar) @?= show (show litChar)
        show (show litString) @?= show (show litString)
    , testCase "Var Eq and Hashable" $ do
        someVar == someVar @?= True
        someVar /= someVar @?= False
        someVar /= otherVar @?= True
        hash someVar @?= hash someVar
    , testCase "Expr show is stable" $ do
        show (show exprTree) @?= show (show exprTree)
        show (show matchExpr) @?= show (show matchExpr)
        show (show letExpr) @?= show (show letExpr)
    , testCase "Bind show is stable" $ do
        show (show nonRecBind) @?= show (show nonRecBind)
        show (show recBind) @?= show (show recBind)
    ]

coreUnitTests :: TestTree
coreUnitTests =
  testGroup "Core Unit tests" [derivedInstanceTests, showGoldens]
