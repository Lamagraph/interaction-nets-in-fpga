module Lamagraph.Compiler.Syntax.SyntaxInstancesTest (syntaxInstancesTests) where

import Relude hiding (show)

import Data.Hashable (hash)
import Prelude (show)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.TestGen

showEqProperty :: (Show a, Eq a) => Gen a -> Property
showEqProperty gen = withTests 50 . property $ do
  x <- forAll gen
  let notEq = x /= x
  assert (show (show x) == show (show x) && x == x && not notEq)

prop_showEqLmlModule :: Property
prop_showEqLmlModule = showEqProperty genModule

prop_showEqLmlDecl :: Property
prop_showEqLmlDecl = showEqProperty genLmlDecl

prop_showEqOpenDecl :: Property
prop_showEqOpenDecl = showEqProperty genOpenDecl

prop_showEqTyDecl :: Property
prop_showEqTyDecl = showEqProperty genTyDecl

prop_showEqConDecl :: Property
prop_showEqConDecl = showEqProperty genConDecl

prop_showEqLmlExpr :: Property
prop_showEqLmlExpr = showEqProperty genLmlExpr

prop_showEqLmlBindGroup :: Property
prop_showEqLmlBindGroup = showEqProperty genLmlBindGroup

prop_showEqLmlBind :: Property
prop_showEqLmlBind = showEqProperty genLmlBind

prop_showEqLmlCase :: Property
prop_showEqLmlCase = showEqProperty genLmlCase

prop_showEqLmlLit :: Property
prop_showEqLmlLit = showEqProperty genLmlLit

prop_showEqLmlPat :: Property
prop_showEqLmlPat = showEqProperty genLmlPat

prop_showEqLmlType :: Property
prop_showEqLmlType = showEqProperty genLmlType

prop_showEqRecFlag :: Property
prop_showEqRecFlag =
  showEqProperty (Gen.choice [pure Recursive, pure NonRecursive])

prop_showOrdHashLongident :: Property
prop_showOrdHashLongident = withTests 50 . property $ do
  l <- forAll (genLongident genIdent)
  let cmp = l `compare` l
      h = hash l
  assert (show (show l) == show (show l) && l == l && cmp == EQ && h == hash l)

prop_lmlModuleFields :: Property
prop_lmlModuleFields = withTests 50 . property $ do
  m <- forAll genModule
  let _ = _lmlModExt m
      _ = _lmlModName m
  assert (length (_lmlModDecls m) == length (_lmlModDecls m))

prop_conDeclFields :: Property
prop_conDeclFields = withTests 50 . property $ do
  c <- forAll genConDecl
  let _ = _cdExt c
      _ = _cdConName c
  assert (length (_cdArgs c) == length (_cdArgs c))

prop_showNoExtField :: Property
prop_showNoExtField = property $
  assert (show noExtField == show noExtField && noExtField == noExtField)

syntaxInstanceProperties :: TestTree
syntaxInstanceProperties =
  testGroup
    "Syntax instance properties"
    [ testProperty "LmlModule show/Eq" prop_showEqLmlModule
    , testProperty "LmlDecl show/Eq" prop_showEqLmlDecl
    , testProperty "OpenDecl show/Eq" prop_showEqOpenDecl
    , testProperty "TyDecl show/Eq" prop_showEqTyDecl
    , testProperty "ConDecl show/Eq" prop_showEqConDecl
    , testProperty "LmlExpr show/Eq" prop_showEqLmlExpr
    , testProperty "LmlBindGroup show/Eq" prop_showEqLmlBindGroup
    , testProperty "LmlBind show/Eq" prop_showEqLmlBind
    , testProperty "LmlCase show/Eq" prop_showEqLmlCase
    , testProperty "LmlLit show/Eq" prop_showEqLmlLit
    , testProperty "LmlPat show/Eq" prop_showEqLmlPat
    , testProperty "LmlType show/Eq" prop_showEqLmlType
    , testProperty "RecFlag show/Eq" prop_showEqRecFlag
    , testProperty "Longident show/Eq/Ord/Hashable" prop_showOrdHashLongident
    , testProperty "LmlModule fields" prop_lmlModuleFields
    , testProperty "ConDecl fields" prop_conDeclFields
    , testProperty "NoExtField show/Eq" prop_showNoExtField
    ]

syntaxInstancesTests :: TestTree
syntaxInstancesTests =
  testGroup "Syntax" [syntaxInstanceProperties]
