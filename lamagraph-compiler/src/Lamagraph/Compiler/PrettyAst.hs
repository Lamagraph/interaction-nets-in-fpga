{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Orphan instances for pretty-printing AST

__Warning__: DO NOT import together with "Lamagraph.Compiler.PrettyLml"!
-}
module Lamagraph.Compiler.PrettyAst () where

import Relude

import Control.Lens hiding (Empty)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Interpolate
import Prettyprinter
import Prettyprinter.Internal

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

----------------------
-- Helper functions --
----------------------

spaceNumber :: Int
spaceNumber = 1

-- | Works like a normal 'vsep', but has stupid check for 'Empty' 'Doc' to help with empty lines
smartVsep :: [Doc ann] -> Doc ann
smartVsep = vsep . filter (\case Empty -> False; _ -> True)

-- | Works like a normal 'hsep', but has stupid check for 'Empty' 'Doc' to help with redundant spaces
smartHsep :: [Doc ann] -> Doc ann
smartHsep = hsep . filter (\case Empty -> False; _ -> True)

-- Default instance for 'Pretty (Maybe a)' prints empty string for 'Nothing'
prettyMaybe :: (Pretty a) => Maybe a -> Doc ann
prettyMaybe Nothing = parens "Nothing"
prettyMaybe (Just a) = parens $ vsep ["Just", pretty a]

-----------------------
-- Utility instances --
-----------------------

instance Pretty DataConCantHappen where
  pretty :: DataConCantHappen -> Doc ann
  pretty _ = emptyDoc

instance Pretty NoExtField where
  pretty :: NoExtField -> Doc ann
  pretty _ = emptyDoc

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

------------------------
-- Main AST instances --
------------------------

instance Pretty RecFlag where
  pretty :: RecFlag -> Doc ann
  pretty Recursive = "Rec"
  pretty NonRecursive = "NonRec"

instance (ForallLmlDecl Pretty (LmlcPass pass)) => Pretty (LmlDecl (LmlcPass pass)) where
  pretty :: LmlDecl (LmlcPass pass) -> Doc ann
  pretty (OpenD ext decl) = smartVsep ["OpenD", pretty ext, pretty decl]
  pretty (ValD ext bindGroup) = smartVsep ["ValD", pretty ext, pretty bindGroup]
  pretty (TyD ext decls) = smartVsep ["TyD", pretty ext, list $ map pretty (toList decls)]

instance (ForallOpenDecl Pretty (LmlcPass pass)) => Pretty (OpenDecl (LmlcPass pass)) where
  pretty :: OpenDecl (LmlcPass pass) -> Doc ann
  pretty (OpenDecl ext ident) = smartVsep ["OpenDecl", pretty ext, pretty ident]

instance (ForallTyDecl Pretty (LmlcPass pass)) => Pretty (TyDecl (LmlcPass pass)) where
  pretty :: TyDecl (LmlcPass pass) -> Doc ann
  pretty (AliasDecl ext name vars ty) = smartVsep ["AliasDecl", pretty ext, pretty name, list $ map pretty vars, pretty ty]
  pretty (DataDecl ext name vars constrs) = smartVsep ["DataDecl", pretty ext, pretty name, list $ map pretty vars, list $ map pretty constrs]

instance (ForallConDecl Pretty (LmlcPass pass)) => Pretty (ConDecl (LmlcPass pass)) where
  pretty :: ConDecl (LmlcPass pass) -> Doc ann
  pretty (ConDecl ext name args) = smartVsep ["ConDecl", pretty ext, pretty name, list (map pretty args)]

instance (ForallLmlExpr Pretty (LmlcPass pass)) => Pretty (LmlExpr (LmlcPass pass)) where
  pretty :: LmlExpr (LmlcPass pass) -> Doc ann
  pretty (LmlExprIdent ext ident) = smartVsep ["ExprIdent", pretty ext, pretty ident]
  pretty (LmlExprConstant ext constant) = smartHsep ["ExprConstant", pretty ext, pretty constant]
  pretty (LmlExprLet ext bindGroup expr) = smartVsep ["ExprLet", pretty ext, pretty bindGroup, pretty expr]
  pretty (LmlExprFunction ext pat expr) = smartVsep ["ExprFunction", pretty ext, pretty pat, pretty expr]
  pretty (LmlExprApply ext expr exprs) = smartVsep ["ExprApply", pretty ext, pretty expr, list $ map pretty (toList exprs)]
  pretty (LmlExprMatch ext expr cases) = smartVsep ["ExprMatch", pretty ext, pretty expr, list $ map pretty (toList cases)]
  pretty (LmlExprTuple ext expr exprs) = smartVsep ["ExprTuple", pretty ext, list $ map pretty (expr : toList exprs)]
  pretty (LmlExprConstruct ext constr expr) = smartVsep ["ExprConstruct", pretty ext, pretty constr, prettyMaybe expr]
  pretty (LmlExprIfThenElse ext cond t f) = smartVsep ["ExprITE", pretty ext, pretty cond, pretty t, pretty f]
  pretty (LmlExprConstraint ext expr ty) = smartVsep ["ExprConstraint", pretty ext, pretty expr, pretty ty]

instance (ForallLmlBindGroup Pretty (LmlcPass pass)) => Pretty (LmlBindGroup (LmlcPass pass)) where
  pretty :: LmlBindGroup (LmlcPass pass) -> Doc ann
  pretty (LmlBindGroup ext recFlag binds) = smartVsep [pretty ext, pretty recFlag, list $ map pretty (toList binds)]

instance (ForallLmlBind Pretty (LmlcPass pass)) => Pretty (LmlBind (LmlcPass pass)) where
  pretty :: LmlBind (LmlcPass pass) -> Doc ann
  pretty (LmlBind ext pat expr) = smartVsep ["Bind", pretty ext, pretty pat, pretty expr]

instance (ForallLmlCase Pretty (LmlcPass pass)) => Pretty (LmlCase (LmlcPass pass)) where
  pretty :: LmlCase (LmlcPass pass) -> Doc ann
  pretty (LmlCase ext pat constraint expr) = smartVsep ["Case", pretty ext, pretty pat, prettyMaybe constraint, pretty expr]

instance (ForallLmlLit Pretty (LmlcPass pass)) => Pretty (LmlLit (LmlcPass pass)) where
  pretty :: LmlLit (LmlcPass pass) -> Doc ann
  pretty (LmlInt ext int) = smartVsep [pretty ext, pretty int]
  pretty (LmlChar ext char) = smartVsep [pretty ext, squotes $ pretty char]
  pretty (LmlString ext str) = smartVsep [pretty ext, dquotes $ pretty str]

instance Pretty Longident where
  pretty :: Longident -> Doc ann
  pretty (Longident idents) = dquotes $ hsep $ punctuate comma (map pretty (toList idents))

instance (ForallLmlPat Pretty (LmlcPass pass)) => Pretty (LmlPat (LmlcPass pass)) where
  pretty :: LmlPat (LmlcPass pass) -> Doc ann
  pretty (LmlPatAny ext) = smartVsep ["PatAny", pretty ext]
  pretty (LmlPatVar ext var) = smartVsep ["PatVar", pretty ext, pretty var]
  pretty (LmlPatConstant ext constant) = smartVsep ["PatConstant", pretty ext, pretty constant]
  pretty (LmlPatTuple ext pat pats) = smartVsep ["PatTuple", pretty ext, list $ map pretty (pat : toList pats)]
  pretty (LmlPatConstruct ext constr pat) = smartVsep ["PatConstruct", pretty ext, pretty constr, prettyMaybe pat]
  pretty (LmlPatOr ext pat1 pat2) = smartVsep ["ParOr", pretty ext, pretty pat1, pretty pat2]
  pretty (LmlPatConstraint ext pat ty) = smartVsep ["PatConstraint", pretty ext, pretty pat, pretty ty]

instance (ForallLmlType Pretty (LmlcPass pass)) => Pretty (LmlType (LmlcPass pass)) where
  pretty :: LmlType (LmlcPass pass) -> Doc ann
  pretty (LmlTyVar ext var) = smartVsep ["TyVar", pretty ext, pretty var]
  pretty (LmlTyArrow ext ty1 ty2) = smartVsep ["TyArrow", pretty ext, pretty ty1, pretty ty2]
  pretty (LmlTyTuple ext ty tys) = smartVsep ["TyTuple", pretty ext, list $ map pretty (ty : toList tys)]
  pretty (LmlTyConstr ext constr tys) = smartVsep ["TyConstr", pretty ext, pretty constr, list (map pretty tys)]

instance (ForallLmlModule Pretty (LmlcPass pass)) => Pretty (LmlModule (LmlcPass pass)) where
  pretty :: LmlModule (LmlcPass pass) -> Doc ann
  pretty (LmlModule ext name decls) = parens $ nest spaceNumber inner
   where
    inner = smartVsep ["Module", pretty ext, prettyMaybe name, list $ map pretty decls]

---------------------------
-- Typechecker instances --
---------------------------

instance (Pretty a) => Pretty (Either TypecheckError a) where
  pretty :: Either TypecheckError a -> Doc ann
  pretty = \case
    Left err -> pretty err
    Right a -> pretty a

instance Pretty TypecheckError where
  pretty :: TypecheckError -> Doc ann
  pretty = \case
    UnboundVariable name -> [i|Error: variable #{pretty name} is unbound|]
    ConstructorDoesntExist name -> [i|Error: constructor #{pretty name} isn't declared|]
    OccursCheck name ty -> [i|Error: variable #{pretty name} already present in type #{pretty ty}|]
    CantUnify lTy rTy -> [i|Error: cannot unify #{pretty lTy} and #{pretty rTy}|]
    NonVariableInLetRec -> [i|Error: non variable pattern in let rec|]
    VariableClashInPattern name -> [i|Error: variable #{pretty name} is already bound in this pattern|]
    VarMustOccurOnBothSidesOfOrPattern name -> [i|Error: variable #{pretty name} must occur on both sides of the or-pattern|]

instance Pretty ModuleResolverError where
  pretty :: ModuleResolverError -> Doc ann
  pretty = \case
    NameNotFound name -> [i|Error: Name #{pretty name} not found|]
    ModuleNotFound name -> [i|Error: Module #{pretty name} not found|]
    ConstructorNotFound name -> [i|Error: Constructor #{pretty name} not found|]

instance Pretty TyEnv where
  pretty :: TyEnv -> Doc ann
  pretty (TyEnv tyEnv) = vsep $ fmap pretty (HashMap.toList tyEnv)

instance {-# OVERLAPS #-} Pretty (Name, TyScheme) where
  pretty :: (Name, TyScheme) -> Doc ann
  pretty (name, tyScheme) = pretty name <> ":" <+> pretty tyScheme

instance Pretty TyScheme where
  pretty :: TyScheme -> Doc ann
  pretty (Forall [] ty) = pretty ty
  pretty (Forall names ty) = "forall" <+> hsep (fmap (\name -> "'" <> pretty name) names) <> "." <+> pretty ty

instance Pretty FullName where
  pretty :: FullName -> Doc ann
  pretty (FullName (Longident idents)) = hsep $ punctuate comma (map pretty (toList idents))

instance Pretty Name where
  pretty :: Name -> Doc ann
  -- TODO: Decide whether this really should get into intrinsics of 'Longident'
  -- or we must have another type in AST that must be quoted
  pretty (Name (Longident idents)) = hsep $ punctuate comma (map pretty (toList idents))

instance Pretty Ty where
  pretty :: Ty -> Doc ann
  pretty = \case
    TVar var -> "'" <> pretty var
    lTy@(_ `TArrow` _) `TArrow` rTy -> parens (pretty lTy) <+> "->" <+> pretty rTy
    lTy `TArrow` rTy -> pretty lTy <+> "->" <+> pretty rTy
    TConstr tyConstr [] -> pretty tyConstr
    TConstr tyConstr [ty] -> pretty ty <+> pretty tyConstr
    TConstr tyConstr tys -> parens (fillSep $ punctuate comma (map pretty tys)) <+> pretty tyConstr
    TTuple ty tys -> parens $ concatWith (surround " * ") (map pretty (ty : toList tys))
