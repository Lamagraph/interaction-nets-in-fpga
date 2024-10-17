{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for pretty-printing AST
module Lamagraph.Compiler.PrettyAST () where

import Relude

import Control.Lens
import Prettyprinter

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

spaceNumber :: Int
spaceNumber = 1

-- Default instance for 'Pretty (Maybe a)' prints empty string for 'Nothing'
prettyMaybe :: (Pretty a) => Maybe a -> Doc ann
prettyMaybe Nothing = parens "Nothing"
prettyMaybe (Just a) = parens $ vsep ["Just", pretty a]

instance Pretty RealSrcSpan where
  pretty :: RealSrcSpan -> Doc ann
  pretty rss = pretty _srcSpanFile <> ":" <> lineCol
   where
    _srcSpanFile = rss ^. srcSpanFile
    _srcSpanSLine = rss ^. srcSpanSLine
    _srcSpanSColumn = rss ^. srcSpanSColumn
    _srcSpanELine = rss ^. srcSpanELine
    _srcSpanEColumn = rss ^. srcSpanEColumn
    lineCol =
      if _srcSpanSLine == _srcSpanELine
        then pretty _srcSpanSLine <> ":" <> pretty _srcSpanSColumn <> "-" <> pretty _srcSpanEColumn
        else
          parens (pretty _srcSpanSLine <> "," <> pretty _srcSpanSColumn)
            <> "-"
            <> parens (pretty _srcSpanELine <> "," <> pretty _srcSpanEColumn)

instance Pretty UnhelpfulSpanReason where
  pretty :: UnhelpfulSpanReason -> Doc ann
  pretty UnhelpfulGenerated = "<compiler-generated code>"
  pretty (UnhelpfulOther text) = angles $ pretty text

instance Pretty SrcSpan where
  pretty :: SrcSpan -> Doc ann
  pretty ss = braces (space <> inner <> space)
   where
    inner = case ss of
      RealSrcSpan rss -> pretty rss
      UnhelpfulSpan usr -> pretty usr

instance (Pretty a) => Pretty (Located a) where
  pretty :: Located a -> Doc ann
  pretty (L loc a) = parens (align inner)
   where
    inner = vsep ["L", pretty loc, parens $ nest spaceNumber $ pretty a]

instance {-# OVERLAPPING #-} Pretty (Located Text) where
  pretty :: Located Text -> Doc ann
  pretty (L loc text) = parens (align inner)
   where
    inner = vsep ["L", pretty loc, dquotes $ pretty text]

instance Pretty RecFlag where
  pretty :: RecFlag -> Doc ann
  pretty Recursive = "Rec"
  pretty NonRecursive = "NonRec"

instance Pretty (LmlDecl (LmlcPass pass)) where
  pretty :: LmlDecl (LmlcPass pass) -> Doc ann
  pretty (OpenD _ decl) = vsep ["OpenD", pretty decl]
  pretty (ValD _ recFlag binds) = vsep ["ValD" <+> pretty recFlag, list $ map pretty (toList binds)]
  pretty (TyD _ decls) = vsep ["TyD", list $ map pretty (toList decls)]

instance Pretty (OpenDecl (LmlcPass pass)) where
  pretty :: OpenDecl (LmlcPass pass) -> Doc ann
  pretty (OpenDecl _ ident) = vsep ["OpenDecl", pretty ident]

instance Pretty (TyDecl (LmlcPass pass)) where
  pretty :: TyDecl (LmlcPass pass) -> Doc ann
  pretty (AliasDecl _ name vars ty) = vsep ["AliasDecl", pretty name, list $ map pretty vars, pretty ty]
  pretty (DataDecl _ name vars constrs) = vsep ["DataDecl", pretty name, list $ map pretty vars, list $ map pretty constrs]

instance Pretty (ConDecl (LmlcPass pass)) where
  pretty :: ConDecl (LmlcPass pass) -> Doc ann
  pretty (ConDecl _ name args) = vsep ["ConDecl", pretty name, list (map pretty args)]

instance Pretty (LmlExpr (LmlcPass pass)) where
  pretty :: LmlExpr (LmlcPass pass) -> Doc ann
  pretty (LmlExprIdent _ ident) = vsep ["ExprIdent", pretty ident]
  pretty (LmlExprConstant _ constant) = "ExprConstant" <+> pretty constant
  pretty (LmlExprLet _ recFlag binds expr) = vsep ["ExprLet" <+> pretty recFlag, list $ map pretty (toList binds), pretty expr]
  pretty (LmlExprFunction _ pat expr) = vsep ["ExprFunction", pretty pat, pretty expr]
  pretty (LmlExprApply _ expr exprs) = vsep ["ExprApply", pretty expr, list $ map pretty (toList exprs)]
  pretty (LmlExprMatch _ expr cases) = vsep ["ExprMatch", pretty expr, list $ map pretty (toList cases)]
  pretty (LmlExprTuple _ expr exprs) = vsep ["ExprTuple", list $ map pretty (expr : toList exprs)]
  pretty (LmlExprConstruct _ constr expr) = vsep ["ExprConstruct", pretty constr, prettyMaybe expr]
  pretty (LmlExprIfThenElse _ cond t f) = vsep ["ExprITE", pretty cond, pretty t, pretty f]
  pretty (LmlExprConstraint _ expr ty) = vsep ["ExprConstraint", pretty expr, pretty ty]

instance Pretty (LmlBind (LmlcPass pass)) where
  pretty :: LmlBind (LmlcPass pass) -> Doc ann
  pretty (LmlBind _ pat expr) = vsep ["Bind", pretty pat, pretty expr]

instance Pretty (LmlCase (LmlcPass pass)) where
  pretty :: LmlCase (LmlcPass pass) -> Doc ann
  pretty (LmlCase _ pat constraint expr) = vsep ["Case", pretty pat, prettyMaybe constraint, pretty expr]

instance Pretty (LmlLit (LmlcPass pass)) where
  pretty :: LmlLit (LmlcPass pass) -> Doc ann
  pretty (LmlInt _ int) = pretty int
  pretty (LmlInt32 _ int32) = pretty int32 <> "l"
  pretty (LmlUInt32 _ uint32) = pretty uint32 <> "ul"
  pretty (LmlInt64 _ int64) = pretty int64 <> "L"
  pretty (LmlUInt64 _ uint64) = pretty uint64 <> "UL"
  pretty (LmlChar _ char) = squotes $ pretty char
  pretty (LmlString _ str) = dquotes $ pretty str

instance Pretty Longident where
  pretty :: Longident -> Doc ann
  pretty (Longident idents) = dquotes $ hsep $ punctuate comma (map pretty (toList idents))

instance Pretty (LmlPat (LmlcPass pass)) where
  pretty :: LmlPat (LmlcPass pass) -> Doc ann
  pretty (LmlPatAny _) = "PatAny"
  pretty (LmlPatVar _ var) = vsep ["PatVar", pretty var]
  pretty (LmlPatConstant _ constant) = vsep ["PatConstant", pretty constant]
  pretty (LmlPatTuple _ pat pats) = vsep ["PatTuple", list $ map pretty (pat : toList pats)]
  pretty (LmlPatConstruct _ constr pat) = vsep ["PatConstruct", pretty constr, prettyMaybe pat]
  pretty (LmlPatOr _ pat1 pat2) = vsep ["ParOr", pretty pat1, pretty pat2]
  pretty (LmlPatConstraint _ pat ty) = vsep ["PatConstraint", pretty pat, pretty ty]

instance Pretty (LmlType (LmlcPass pass)) where
  pretty :: LmlType (LmlcPass pass) -> Doc ann
  pretty (LmlTyVar _ var) = vsep ["TyVar", pretty var]
  pretty (LmlTyArrow _ ty1 ty2) = vsep ["TyArrow", pretty ty1, pretty ty2]
  pretty (LmlTyTuple _ ty tys) = vsep ["TyTuple", list $ map pretty (ty : toList tys)]
  pretty (LmlTyConstr _ constr tys) = vsep ["TyConstr", pretty constr, list (map pretty tys)]

instance Pretty (LmlModule (LmlcPass pass)) where
  pretty :: LmlModule (LmlcPass pass) -> Doc ann
  pretty (LmlModule _ name decls) = parens $ nest spaceNumber inner
   where
    inner = vsep ["Module", prettyMaybe name, list $ map pretty decls]
