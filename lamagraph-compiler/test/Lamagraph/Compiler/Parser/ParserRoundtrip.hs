{-# LANGUAGE RecordWildCards #-}

module Lamagraph.Compiler.Parser.ParserRoundtrip (prop_ParserRoundtrip) where

import Relude

import Data.List.NonEmpty.Extra qualified as NE
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prettyprinter
import Prettyprinter.Render.Text

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.PrettyLML ()
import Lamagraph.Compiler.Syntax

{- | '<>' lifted to 'Applicative'
Very useful in this module because of 'Text' concatenation under 'Gen' monad
-}
(.<>.) :: (Applicative f, Semigroup c) => f c -> f c -> f c
a .<>. b = liftA2 (<>) a b

keywords :: [Text]
keywords =
  [ "and"
  , "asr"
  , "else"
  , "false"
  , "fun"
  , "if"
  , "in"
  , "land"
  , "let"
  , "lor"
  , "lsl"
  , "lsr"
  , "lxor"
  , "match"
  , "mod"
  , "module"
  , "of"
  , "open"
  , "rec"
  , "then"
  , "true"
  , "type"
  , "when"
  , "with"
  ]

isKeyword :: Text -> Bool
isKeyword word = word `elem` keywords

notKeyword :: Text -> Bool
notKeyword = not . isKeyword

-- | Mostly 'Gen.alphaNum', but with @_@
identChar :: Gen Char
identChar = Gen.element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

identRange :: Range Int
identRange = Range.linear 0 11

nonEmptyRange :: Range Int
nonEmptyRange = Range.linear 1 6

listRange :: Range Int
listRange = Range.linear 0 6

declRange :: Range Int
declRange = Range.linear 0 200

{-# INLINE mkGenLoc #-}
mkGenLoc :: a -> Located a
mkGenLoc = L generatedSrcSpan

genCapitalizedIdent :: Gen Text
genCapitalizedIdent = (T.singleton <$> Gen.upper) .<>. Gen.text identRange identChar

genLCapitalizedIdent :: Gen (Located Text)
genLCapitalizedIdent = mkGenLoc <$> genCapitalizedIdent

genLowercaseIdent :: Gen Text
genLowercaseIdent =
  Gen.choice
    [ Gen.filter notKeyword identStartingLetter
    , Gen.constant "_" .<>. Gen.text (Range.linear 1 1) Gen.alphaNum .<>. Gen.text identRange identChar
    ]
 where
  identStartingLetter = (T.singleton <$> Gen.lower) .<>. Gen.text identRange identChar

genLLowercaseIdent :: Gen (Located Text)
genLLowercaseIdent = mkGenLoc <$> genLowercaseIdent

genLongident :: Gen Text -> Gen Longident
genLongident genLastIdent = do
  modulePath <- Gen.nonEmpty nonEmptyRange genCapitalizedIdent
  mkLongident . NE.snoc modulePath <$> genLastIdent

genLLongident :: Gen Text -> Gen (LLongident LmlcPs)
genLLongident genLastIdent = mkGenLoc <$> genLongident genLastIdent

genValueName :: Gen Text
genValueName = Gen.choice [genLowercaseIdent, genPrefixSymbol, genInfixSymbolFiltered]
 where
  opTailRange = Range.linear 0 5
  genOpChar = Gen.element "!$%&*+./:<=>?@^|~"
  genPrefixSymbol =
    Gen.choice
      [ pure "!" .<>. Gen.text opTailRange genOpChar
      , Gen.choice [pure "?", pure "~"] .<>. Gen.text (Range.linear 1 5) genOpChar
      ]
  genInfixSymbol = (T.singleton <$> Gen.element "=<>@^|&+-*/$%") .<>. Gen.text opTailRange genOpChar
  genInfixSymbolFiltered = Gen.filter (\x -> not (T.isPrefixOf "|" x || T.isPrefixOf "->" x)) genInfixSymbol

genLValueName :: Gen (Located Text)
genLValueName = mkGenLoc <$> genValueName

genIdent :: Gen Text
genIdent =
  Gen.choice
    [ genLowercaseIdent
    , genCapitalizedIdent
    ]

genLIdent :: Gen (Located Text)
genLIdent = mkGenLoc <$> genIdent

genChar :: Gen Char
genChar = Gen.enum '\32' '\126'

genLmlDecl :: Gen (LmlDecl LmlcPs)
genLmlDecl = do
  Gen.choice
    [ OpenD noExtField <$> genOpenDecl
    , genValD
    , TyD noExtField <$> Gen.nonEmpty nonEmptyRange genLTyDecl
    ]

genValD :: Gen (LmlDecl LmlcPs)
genValD = do
  binds <- Gen.nonEmpty nonEmptyRange genLLmlBind
  flag <- Gen.element [NonRecursive, Recursive]
  pure $ ValD noExtField flag binds

genLLmlDecl :: Gen (LLmlDecl LmlcPs)
genLLmlDecl = mkGenLoc <$> genLmlDecl

genOpenDecl :: Gen (OpenDecl LmlcPs)
genOpenDecl = do
  OpenDecl noExtField <$> genLLongident genCapitalizedIdent

genTyVars :: Gen [LLmlType LmlcPs]
genTyVars = Gen.list listRange (mkGenLoc . LmlTyVar noExtField <$> genLIdent)

genTyDecl :: Gen (TyDecl LmlcPs)
genTyDecl =
  Gen.choice
    [ AliasDecl noExtField <$> genLLowercaseIdent <*> genTyVars <*> genLLmlType
    , DataDecl noExtField <$> genLLowercaseIdent <*> genTyVars <*> Gen.list listRange genLConDecl
    ]

genLTyDecl :: Gen (LTyDecl LmlcPs)
genLTyDecl = mkGenLoc <$> genTyDecl

genConDecl :: Gen (ConDecl LmlcPs)
genConDecl = ConDecl noExtField <$> genLCapitalizedIdent <*> Gen.list listRange genLLmlType

genLConDecl :: Gen (LConDecl LmlcPs)
genLConDecl = mkGenLoc <$> genConDecl

genLmlExpr :: Gen (LmlExpr LmlcPs)
genLmlExpr =
  Gen.recursive
    Gen.choice
    [ LmlExprIdent noExtField <$> genLongident genValueName
    , LmlExprConstant noExtField <$> genLmlLit
    ]
    [ LmlExprLet noExtField
        <$> Gen.element [NonRecursive, Recursive]
        <*> Gen.nonEmpty nonEmptyRange genLLmlBind
        <*> genLLmlExpr
    , LmlExprFunction noExtField <$> genLLmlPat <*> genLLmlExpr
    , let func = (mkGenLoc . LmlExprIdent noExtField <$> genLongident genValueName)
       in LmlExprApply noExtField <$> func <*> Gen.nonEmpty nonEmptyRange genLLmlExpr
    , LmlExprMatch noExtField <$> genLLmlExpr <*> Gen.nonEmpty nonEmptyRange genLLmlCase
    , LmlExprTuple noExtField <$> genLLmlExpr <*> Gen.nonEmpty nonEmptyRange genLLmlExpr
    , LmlExprConstruct noExtField <$> genLLongident genCapitalizedIdent <*> Gen.maybe genLLmlExpr
    , LmlExprIfThenElse noExtField <$> genLLmlExpr <*> genLLmlExpr <*> genLLmlExpr
    , LmlExprConstraint noExtField <$> genLLmlExpr <*> genLLmlType
    ]

genLLmlExpr :: Gen (LLmlExpr LmlcPs)
genLLmlExpr = mkGenLoc <$> genLmlExpr

genLmlBind :: Gen (LmlBind LmlcPs)
genLmlBind = LmlBind noExtField <$> genLLmlPat <*> genLLmlExpr

genLLmlBind :: Gen (LLmlBind LmlcPs)
genLLmlBind = mkGenLoc <$> genLmlBind

genLmlCase :: Gen (LmlCase LmlcPs)
genLmlCase = LmlCase noExtField <$> genLLmlPat <*> Gen.maybe genLLmlExpr <*> genLLmlExpr

genLLmlCase :: Gen (LLmlCase LmlcPs)
genLLmlCase = mkGenLoc <$> genLmlCase

genLmlLit :: Gen (LmlLit LmlcPs)
genLmlLit =
  Gen.choice
    [ LmlInt noExtField <$> Gen.int (Range.linear minBound maxBound)
    , LmlInt32 noExtField <$> Gen.int32 (Range.linear minBound maxBound)
    , LmlUInt32 noExtField <$> Gen.word32 (Range.linear minBound maxBound)
    , LmlInt64 noExtField <$> Gen.int64 (Range.linear minBound maxBound)
    , LmlUInt64 noExtField <$> Gen.word64 (Range.linear minBound maxBound)
    , LmlChar noExtField <$> genChar
    , LmlString noExtField <$> Gen.text (Range.linear 0 25) genChar
    ]

genLmlPat :: Gen (LmlPat LmlcPs)
genLmlPat =
  Gen.recursive
    Gen.choice
    [ pure $ LmlPatAny noExtField
    , LmlPatVar noExtField <$> genLValueName
    , LmlPatConstant noExtField <$> genLmlLit
    ]
    [ LmlPatTuple noExtField <$> genLLmlPat <*> Gen.nonEmpty nonEmptyRange genLLmlPat
    , LmlPatConstruct noExtField <$> genLLongident genCapitalizedIdent <*> Gen.maybe genLLmlPat
    , LmlPatOr noExtField <$> genLLmlPat <*> genLLmlPat
    , LmlPatConstraint noExtField <$> genLLmlPat <*> genLLmlType
    ]

genLLmlPat :: Gen (LLmlPat LmlcPs)
genLLmlPat = mkGenLoc <$> genLmlPat

genLmlType :: Gen (LmlType LmlcPs)
genLmlType =
  Gen.recursive
    Gen.choice
    [LmlTyVar noExtField <$> genLIdent]
    [ LmlTyArrow noExtField <$> genLLmlType <*> genLLmlType
    , LmlTyTuple noExtField <$> genLLmlType <*> Gen.nonEmpty nonEmptyRange genLLmlType
    , LmlTyConstr noExtField <$> genLLongident genLowercaseIdent <*> Gen.list listRange genLLmlType
    ]

genLLmlType :: Gen (LLmlType LmlcPs)
genLLmlType = mkGenLoc <$> genLmlType

genModule :: Gen (LmlModule LmlcPs)
genModule = do
  let _lmlModExt = noExtField
  _lmlModName <- Gen.maybe $ genLLongident genCapitalizedIdent
  _lmlModDecls <- Gen.list declRange genLLmlDecl
  pure LmlModule{..}

prop_ParserRoundtrip :: Property
prop_ParserRoundtrip = withTests 100 . property $ do
  asts <- forAll genModule
  tripping asts (renderStrict . layoutPretty defaultLayoutOptions . pretty) parseLamagraphML
