{-# OPTIONS_GHC -Wno-orphans #-}

module Lamagraph.Compiler.Core.Pretty () where

import Relude

import Lamagraph.Compiler.Core

import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Typechecker.TcTypes
import Prettyprinter

-- TODO: These instances will conflict with "PrettyAST"!

instance Pretty Longident where
  pretty :: Longident -> Doc ann
  pretty (Longident idents) = hsep $ punctuate comma (map pretty (toList idents))

instance Pretty Name where
  pretty :: Name -> Doc ann
  pretty (Name longident) = pretty longident

instance Pretty Var where
  pretty :: Var -> Doc ann
  pretty (Id name) = pretty name

instance Pretty CoreBind where
  pretty :: CoreBind -> Doc ann
  pretty = \case
    NonRec var expr -> "let" <+> helper var expr
    Rec binds -> "let rec" <+> concatWith (surround (hardline <> "and" <> hardline)) (map inner (toList binds))
     where
      inner (var, expr) = helper var expr
   where
    helper var expr = pretty var <+> "=" <+> align (pretty expr)
  prettyList :: [CoreBind] -> Doc ann
  prettyList binds = vsep $ map pretty binds

instance Pretty CoreExpr where
  pretty :: CoreExpr -> Doc ann
  pretty = \case
    Var var -> pretty var
    Lit lit -> pretty lit
    App expr1 expr2 -> helper expr1 <+> helper expr2
     where
      helper expr = case expr of
        var@(Var{}) -> pretty var
        lit@(Lit{}) -> pretty lit
        tuple@(Tuple{}) -> pretty tuple
        other -> parens $ pretty other
    Lam var expr -> "fun" <+> pretty var <+> "->" <> softline <> pretty expr
    Let bind expr -> pretty bind <> softline <> "in" <+> pretty expr
    Match scrutinee scrutineeVar alts ->
      align $
        "match"
          <+> pretty scrutinee
          <+> "as"
          <+> pretty scrutineeVar
          <+> "with"
          <+> encloseSep emptyDoc emptyDoc (flatAlt "| " " | ") (map pretty (toList alts))
    Tuple expr exprs -> parens (fillSep $ punctuate comma (map (parens . pretty) (expr : toList exprs)))

instance Pretty Literal where
  pretty :: Literal -> Doc ann
  pretty = \case
    LitInt int -> pretty int
    LitChar char -> squotes $ pretty char
    LitString string -> dquotes $ pretty string

instance Pretty AltCon where
  pretty :: AltCon -> Doc ann
  pretty = \case
    DataAlt dataCon -> pretty dataCon
    LitAlt lit -> pretty lit
    TupleAlt -> "TUPLE"
    DEFAULT -> "DEFAULT"
