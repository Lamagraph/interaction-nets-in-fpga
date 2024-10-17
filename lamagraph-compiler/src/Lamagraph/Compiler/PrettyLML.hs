{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for pretty-printing LamagraphML
module Lamagraph.Compiler.PrettyLML () where

import Relude

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Prettyprinter

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

prettyADTVar :: LLmlType (LmlcPass pass) -> Doc ann
prettyADTVar (L _ (LmlTyVar _ varName)) = "'" <> pretty varName
prettyADTVar t =
  error
    [i|Internal pretty-printer error: expected list of type variables in AST as a type parameters, but got #{pretty t}|]

prettyADTVars :: [LLmlType (LmlcPass pass)] -> Doc ann
prettyADTVars [] = emptyDoc
prettyADTVars [var] = prettyADTVar var <> space
prettyADTVars vars = parens (fillSep $ punctuate comma (map prettyADTVar vars)) <> space

prettyChar :: Char -> Doc ann
prettyChar '\\' = "\\\\"
prettyChar '\"' = "\\\""
prettyChar '\'' = "\\\'"
prettyChar '\n' = "\\\n"
prettyChar c
  | c `elem` ['\32' .. '\127'] = pretty c
  | otherwise = error [i|Internal pretty-printer error: trying to print unsupported in LML character "#{c}"|]

prettyString :: Text -> Doc ann
prettyString str = mconcat $ map prettyChar (toString str)

instance (Pretty a) => Pretty (Located a) where
  pretty :: Located a -> Doc ann
  pretty (L _ a) = pretty a

instance Pretty RecFlag where
  pretty :: RecFlag -> Doc ann
  pretty Recursive = "rec" <> space
  pretty NonRecursive = emptyDoc

instance Pretty (LmlDecl (LmlcPass pass)) where
  pretty :: LmlDecl (LmlcPass pass) -> Doc ann
  pretty (OpenD _ decl) = "open" <+> pretty decl
  pretty (ValD _ recFlag binds) = "let" <+> pretty recFlag <> concatWith (surround (hardline <> "and" <> hardline)) (map pretty (toList binds))
  pretty (TyD _ decls) = "type" <+> concatWith (surround (hardline <> "and" <> hardline)) (map pretty (toList decls))

instance Pretty (OpenDecl (LmlcPass pass)) where
  pretty :: OpenDecl (LmlcPass pass) -> Doc ann
  pretty (OpenDecl _ ident) = pretty ident

instance Pretty (TyDecl (LmlcPass pass)) where
  pretty :: TyDecl (LmlcPass pass) -> Doc ann
  pretty (AliasDecl _ name vars ty) = prettyADTVars vars <> pretty name <+> "=" <> softline <> pretty ty
  pretty (DataDecl _ name vars []) = prettyADTVars vars <> pretty name
  pretty (DataDecl _ name vars constrs) =
    prettyADTVars vars
      <> pretty name
      <+> align ("=" <+> encloseSep emptyDoc emptyDoc (flatAlt "| " " | ") (map pretty constrs))

instance Pretty (ConDecl (LmlcPass pass)) where
  pretty :: ConDecl (LmlcPass pass) -> Doc ann
  pretty (ConDecl _ (L _ "::") args) = "(::)" <+> "of" <+> concatWith (surround " * ") (map pretty args)
  pretty (ConDecl _ name args) =
    pretty name <> case args of
      [] -> emptyDoc
      _ -> space <> "of" <+> concatWith (surround " * ") (map pretty args)

instance Pretty (LmlExpr (LmlcPass pass)) where
  pretty :: LmlExpr (LmlcPass pass) -> Doc ann
  pretty (LmlExprIdent _ ident) = pretty ident
  pretty (LmlExprConstant _ constant) = pretty constant
  pretty (LmlExprLet _ recFlag binds expr) =
    "let"
      <+> pretty recFlag
      <> concatWith (surround (hardline <> "and" <> hardline)) (map pretty (toList binds))
      <+> "in"
      <+> pretty expr
  pretty (LmlExprFunction _ pat expr) = "fun" <+> pretty pat <+> "->" <+> pretty expr
  pretty (LmlExprApply _ expr exprs) = pretty expr <+> hsep (map (parens . pretty) (toList exprs))
  pretty (LmlExprMatch _ expr cases) =
    parens $
      align
        ("match" <+> pretty expr <+> "with" <+> encloseSep emptyDoc emptyDoc (flatAlt "| " " | ") (map pretty (toList cases)))
  pretty (LmlExprTuple _ expr exprs) = parens (fillSep $ punctuate comma (map (parens . pretty) (expr : toList exprs)))
  pretty (LmlExprConstruct _ (L _ (Longident ("::" :| []))) (Just (L _ (LmlExprTuple _ hd tl)))) = parens (pretty hd <+> "::" <+> pretty (head tl))
  pretty (LmlExprConstruct _ constr Nothing) = pretty constr
  pretty (LmlExprConstruct _ constr (Just expr)) = parens (pretty constr <+> parens (pretty expr))
  pretty (LmlExprIfThenElse _ cond t f) = "if" <+> pretty cond <+> "then" <+> pretty t <+> "else" <+> pretty f
  pretty (LmlExprConstraint _ expr ty) = parens (pretty expr <+> ":" <+> pretty ty)

instance Pretty (LmlBind (LmlcPass pass)) where
  pretty :: LmlBind (LmlcPass pass) -> Doc ann
  pretty (LmlBind _ pat expr) = pretty pat <+> "=" <> softline <> pretty expr

instance Pretty (LmlCase (LmlcPass pass)) where
  pretty :: LmlCase (LmlcPass pass) -> Doc ann
  pretty (LmlCase _ pat Nothing expr) = pretty pat <+> "->" <+> pretty expr
  pretty (LmlCase _ pat (Just constraint) expr) = pretty pat <+> "when" <+> pretty constraint <+> "->" <+> pretty expr

instance Pretty (LmlLit (LmlcPass pass)) where
  pretty :: LmlLit (LmlcPass pass) -> Doc ann
  pretty (LmlInt _ int) = pretty int
  pretty (LmlInt32 _ int32) = pretty int32 <> "l"
  pretty (LmlUInt32 _ uint32) = pretty uint32 <> "ul"
  pretty (LmlInt64 _ int64) = pretty int64 <> "L"
  pretty (LmlUInt64 _ uint64) = pretty uint64 <> "UL"
  pretty (LmlChar _ char) = squotes $ prettyChar char
  pretty (LmlString _ str) = dquotes $ prettyString str

instance Pretty Longident where
  pretty :: Longident -> Doc ann
  pretty (Longident ident) = if res then prettyInit <> parens (space <> pretty func <> space) else prettyInit <> pretty func
   where
    func = last ident
    initList = init ident
    prettyInit = if null initList then emptyDoc else concatWith (surround dot) (map pretty initList) <> dot
    prefixes = ["*", "/", "%", "+", "-", "@", "^", "=", "<", ">", "|", "&", "$", "!", "?", "~"]
    equal = ["lor", "lxor", "mod", "land", "lsl", "lsr", "asr"]
    startsWithB = map (`T.isPrefixOf` func) prefixes
    equalsB = map (func ==) equal
    res = or startsWithB || or equalsB

instance Pretty (LmlPat (LmlcPass pass)) where
  pretty :: LmlPat (LmlcPass pass) -> Doc ann
  pretty (LmlPatAny _) = "_"
  pretty (LmlPatVar _ (L _ var)) = pretty $ mkLongident (pure var)
  pretty (LmlPatConstant _ constant) = pretty constant
  pretty (LmlPatTuple _ pat pats) = parens $ fillSep (punctuate comma (map pretty (pat : toList pats)))
  pretty (LmlPatConstruct _ (L _ (Longident ("::" :| []))) (Just (L _ (LmlPatTuple _ hd tl)))) = parens (pretty hd <+> "::" <+> pretty (head tl))
  pretty (LmlPatConstruct _ constr Nothing) = pretty constr
  pretty (LmlPatConstruct _ constr (Just pat)) = parens (pretty constr <+> parens (pretty pat))
  pretty (LmlPatOr _ pat1 pat2) = parens (pretty pat1 <+> "|" <+> pretty pat2)
  pretty (LmlPatConstraint _ pat ty) = parens (pretty pat <+> ":" <+> pretty ty)

instance Pretty (LmlType (LmlcPass pass)) where
  pretty :: LmlType (LmlcPass pass) -> Doc ann
  pretty (LmlTyVar _ var) = "'" <> pretty var
  pretty (LmlTyArrow _ ty1 ty2) = parens (pretty ty1 <+> "->" <> softline <> pretty ty2)
  pretty (LmlTyTuple _ ty tys) = parens $ concatWith (surround " * ") (map pretty (ty : toList tys))
  pretty (LmlTyConstr _ constr []) = pretty constr
  pretty (LmlTyConstr _ constr [ty@(L _ (LmlTyArrow{}))]) = parens (pretty ty) <+> pretty constr
  pretty (LmlTyConstr _ constr [ty]) = pretty ty <+> pretty constr
  pretty (LmlTyConstr _ constr tys) = parens (fillSep $ punctuate comma (map pretty tys)) <+> pretty constr

instance Pretty (LmlModule (LmlcPass pass)) where
  pretty :: LmlModule (LmlcPass pass) -> Doc ann
  pretty (LmlModule _ name decls) = concatWith (surround (hardline <> hardline)) $ case name of
    Nothing -> map pretty decls
    Just moduleName -> ("module" <+> pretty moduleName) : map pretty decls
