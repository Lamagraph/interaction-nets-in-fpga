{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Lamagraph.Compiler.Extension (Pass (..), LmlcPass (..), LmlcPs) where

import Relude

import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

data Pass = Parsed

data LmlcPass (c :: Pass) where
  LmlcPs :: LmlcPass 'Parsed

type LmlcPs = LmlcPass 'Parsed -- Output of parser

type instance XLocated (LmlcPass p) a = Located a

type instance XCModule (LmlcPass _) = NoExtField
type instance XXModule (LmlcPass _) = DataConCantHappen

type instance XOpenD (LmlcPass _) = NoExtField
type instance XXLmlDecl (LmlcPass _) = DataConCantHappen

type instance XOpenDecl (LmlcPass _) = NoExtField
type instance XXOpenDecl (LmlcPass _) = DataConCantHappen

type instance XLmlTyVar (LmlcPass _) = NoExtField
type instance XLmlTyArrow (LmlcPass _) = NoExtField
type instance XLmlTyTuple (LmlcPass _) = NoExtField
type instance XLmlTyConstr (LmlcPass _) = NoExtField
type instance XXType (LmlcPass _) = DataConCantHappen

type instance XLmlInt (LmlcPass _) = NoExtField
type instance XLmlInt32 (LmlcPass _) = NoExtField
type instance XLmlUInt32 (LmlcPass _) = NoExtField
type instance XLmlInt64 (LmlcPass _) = NoExtField
type instance XLmlUInt64 (LmlcPass _) = NoExtField
type instance XLmlChar (LmlcPass _) = NoExtField
type instance XLmlString (LmlcPass _) = NoExtField
type instance XXLit (LmlcPass _) = DataConCantHappen

type instance XLmlPatAny (LmlcPass _) = NoExtField
type instance XLmlPatVar (LmlcPass _) = NoExtField
type instance XLmlPatConstant (LmlcPass _) = NoExtField
type instance XLmlPatTuple (LmlcPass _) = NoExtField
type instance XLmlPatConstruct (LmlcPass _) = NoExtField
type instance XLmlPatOr (LmlcPass _) = NoExtField
type instance XLmlPatConstraint (LmlcPass _) = NoExtField
type instance XXPat (LmlcPass _) = DataConCantHappen
