{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lamagraph.Compiler.Typechecker.Instances () where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

instance {-# OVERLAPPING #-} (Tys (a LmlcTc)) => Tys (Located (a LmlcTc)) where
  apply :: Subst -> Located (a LmlcTc) -> Located (a LmlcTc)
  apply subst (L pos a) = L pos (apply subst a)
  ftv :: Located (a LmlcTc) -> HashSet Name
  ftv (L _ a) = ftv a

instance (ForallLmlLit Tys LmlcTc) => Tys (LmlLit LmlcTc) where
  apply :: Subst -> LmlLit LmlcTc -> LmlLit LmlcTc
  apply subst = \case
    LmlInt ext int -> LmlInt (apply subst ext) int
    LmlChar ext char -> LmlChar (apply subst ext) char
    LmlString ext string -> LmlString (apply subst ext) string
  ftv :: LmlLit LmlcTc -> HashSet Name
  ftv = \case
    LmlInt ext _ -> ftv ext
    LmlChar ext _ -> ftv ext
    LmlString ext _ -> ftv ext

instance (ForallLmlPat Tys LmlcTc) => Tys (LmlPat LmlcTc) where
  apply :: Subst -> LmlPat LmlcTc -> LmlPat LmlcTc
  apply subst = \case
    LmlPatAny ext -> LmlPatAny (apply subst ext)
    LmlPatVar ext name -> LmlPatVar (apply subst ext) name
    LmlPatConstant ext lit -> LmlPatConstant (apply subst ext) (apply subst lit)
    LmlPatTuple ext pat pats -> LmlPatTuple (apply subst ext) (apply subst pat) (fmap (apply subst) pats)
    LmlPatConstruct ext lLongident maybeLPat -> LmlPatConstruct (apply subst ext) lLongident (fmap (apply subst) maybeLPat)
    LmlPatOr ext lPat1 lPat2 -> LmlPatOr (apply subst ext) (apply subst lPat1) (apply subst lPat2)
    LmlPatConstruct ext lPat lTy -> LmlPatConstruct (apply subst ext) (apply subst lPat) (apply subst lTy)
