{
{- I have no clue whether this make a difference in *any* speed,
but Happy docs recommend this flag -}
{-# OPTIONS_GHC -fglasgow-exts #-}

{- | LamagraphML parser made with Happy
-}
module Lamagraph.Compiler.Parser (parseLamagraphML) where

import Relude

import qualified Data.List.NonEmpty.Extra as NE
import qualified Prelude -- Required for Happy code

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.Lexer
import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
}

%name pLamagraphML module_expression
%tokentype { LToken }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { L _ TokEOF }
%expect 0

%token
  -- Identifiers
  capitalized_ident { L _ (TokIdent Capitalized _)  }
  lowercase_ident { L _ (TokIdent Lowercase _) }
  -- Integer literals
  integer_literal { L _ (TokInt _) }
  -- Character liteal
  char_literal { L _ (TokChar _) }
  -- String literal
  string_literal { L _ (TokString _) }
  -- Operators
  infix_symbol0 { L _ (TokInfixSymbol0 _) }
  infix_symbol1 { L _ (TokInfixSymbol1 _) }
  infix_symbol2 { L _ (TokInfixSymbol2 _) }
  infix_symbol3 { L _ (TokInfixSymbol3 _) }
  infix_symbol4 { L _ (TokInfixSymbol4 _) }
  prefix_symbol { L _ (TokPrefixSymbol _) }
  -- Keywords
  'and' { L _ TokAnd }
  'asr' { L _ TokAsr }
  'else' { L _ TokElse }
  'false' { L _ TokFalse }
  'fun' { L _ TokFun }
  'if' { L _ TokIf }
  'in' { L _ TokIn }
  'land' { L _ TokLand }
  'let' { L _ TokLet }
  'lor' { L _ TokLor }
  'lsl' { L _ TokLsl }
  'lsr' { L _ TokLsr }
  'lxor' { L _ TokLxor }
  'match' { L _ TokMatch }
  'mod' { L _ TokMod }
  'module' { L _ TokModule }
  'of' { L _ TokOf }
  'open' { L _ TokOpen }
  'rec' { L _ TokRec }
  'then' { L _ TokThen }
  'true' { L _ TokTrue }
  'type' { L _ TokType }
  'when' { L _ TokWhen }
  'with' { L _ TokWith }
  '&&' { L _ TokBoolAnd }
  '\'' { L _ TokApostrophe }
  '(' { L _ TokLeftPar }
  ')' { L _ TokRightPar }
  '*' { L _ TokStar }
  '+' { L _ TokPlus }
  ',' { L _ TokComma }
  '-' { L _ TokMinus }
  '->' { L _ TokArrow }
  ':' { L _ TokColon }
  '::' { L _ TokDoubleColon }
  ';' { L _ TokSemicolon }
  '=' { L _ TokEq }
  '[' { L _ TokLeftBracket }
  ']' { L _ TokRightBracket }
  '_' { L _ TokWildcard }
  '.' { L _ TokDot }
  '|' { L _ TokBar }
  '||' { L _ TokDoubleBar }

-- Higher in the list, lower the precendce
%nonassoc 'in'
%nonassoc ';'
%nonassoc 'let'
%nonassoc 'with'
%nonassoc 'and'
%nonassoc 'then'
%nonassoc 'else'
%left '|'
%nonassoc below_COMMA
%left ',' -- typexpr (e, e, e)
%right '->' -- typexpr (t -> t ->t)
%right '||'
%right '&&'
%left '=' infix_symbol0
%right infix_symbol1
%right '::' -- e :: e :: e
%left '+' '-' infix_symbol2
%left '*' infix_symbol3 'lor' 'lxor' 'mod' 'land' -- typexpr ('t * 't)
%right infix_symbol4 'lsl' 'lsr' 'asr'
%nonassoc prec_unary_minus
%nonassoc prec_constant_constructor
%nonassoc constr_appl -- above '|' '::' ','
%nonassoc below_DOT
%nonassoc '.' -- idents
%nonassoc '(' ')' capitalized_ident lowercase_ident integer_literal
          char_literal string_literal 'false' '[' ']' prefix_symbol
          'true' '_'

%%
-- Happy does pattern-matching againt %token directives, thus we need a rule for ident
-- TODO: RdrName?
ident :: { XLocated LmlcPs Text }
  : lowercase_ident { sL1 $1 $ getIdent $1 }
  | capitalized_ident { sL1 $1 $ getIdent $1 }

-----------------
-- Basic names --
-----------------
value_name :: { XLocated LmlcPs Text }
  : lowercase_ident { sL1 $1 $ getIdent $1 }
  | '(' operator_name ')' { sLL $1 $3 $ unLoc $2 }

operator_name :: { XLocated LmlcPs Text }
  : prefix_symbol { sL1 $1 $ getIdent $1 }
  | infix_op { $1 }

infix_op :: { XLocated LmlcPs Text }
  : infix_symbol0 { sL1 $1 $ getIdent $1 }
  | infix_symbol1 { sL1 $1 $ getIdent $1 }
  | infix_symbol2 { sL1 $1 $ getIdent $1 }
  | infix_symbol3 { sL1 $1 $ getIdent $1 }
  | infix_symbol4 { sL1 $1 $ getIdent $1 }
  | '*' { sL1 $1 "*" }
  | '+' { sL1 $1 "+" }
  | '-' { sL1 $1 "-" }
  | '=' { sL1 $1 "=" }
  | '||' { sL1 $1 "||" }
  | '&&' { sL1 $1 "&&" }
  | 'mod' { sL1 $1 "mod" }
  | 'land' { sL1 $1 "land" }
  | 'lor' { sL1 $1 "lor" }
  | 'lxor' { sL1 $1 "lxor" }
  | 'lsl' { sL1 $1 "lsl" }
  | 'lsr' { sL1 $1 "lsr" }
  | 'asr' { sL1 $1 "asr" }

constr_name :: { XLocated LmlcPs Text }
  : capitalized_ident { sL1 $1 $ getIdent $1 }

typeconstr_name :: { XLocated LmlcPs Text }
  : lowercase_ident { sL1 $1 $ getIdent $1 }

---------------------
-- Qualified names --
---------------------
module_pathT :: { NonEmpty LToken }
  : mkIdentRev(module_pathT, capitalized_ident) { $1 }

value_nameT :: { LToken }
  : lowercase_ident { $1 }
  | '(' operator_nameT ')' { sLL $1 $3 $ unLoc $2 }

operator_nameT :: { LToken }
  : prefix_symbol { $1 }
  | infix_opT { $1 }

infix_opT :: { LToken }
  : infix_symbol0 { $1 }
  | infix_symbol1 { $1 }
  | infix_symbol2 { $1 }
  | infix_symbol3 { $1 }
  | infix_symbol4 { $1 }
  | '*' { $1 }
  | '+' { $1 }
  | '-' { $1 }
  | '=' { $1 }
  | '||' { $1 }
  | '&&' { $1 }
  | 'mod' { $1 }
  | 'land' { $1 }
  | 'lor' { $1 }
  | 'lxor' { $1 }
  | 'lsl' { $1 }
  | 'lsr' { $1 }
  | 'asr' { $1 }

value_path :: { LLongident LmlcPs }
  : mkIdentRev(module_pathT, value_nameT) { sLNE $1 $ getLongident (NE.reverse $1) }
  -- | mkIdentRev(module_pathT, ) { sLNE $1 $ getLongident (NE.reverse $1) }

constr :: { LLongident LmlcPs }
  : mkIdentRev(module_pathT, capitalized_ident) { sLNE $1 $ getLongident (NE.reverse $1) }
  | '[' ']' { sLL $1 $2 nilConstruct }
  | '(' ')' { sLL $1 $2 unitConstruct }
  | 'true' { sL1 $1 $ (mkLongident . pure) "true" }
  | 'false' { sL1 $1 $ (mkLongident . pure) "false" }

typeconstr :: { LLongident LmlcPs }
  : mkIdentRev(module_pathT, lowercase_ident) { sLNE $1 $ getLongident (NE.reverse $1) }

module_path :: { LLongident LmlcPs }
  : module_pathT { sLNE $1 $ getLongident (NE.reverse $1) }

----------------------
-- Type expressions --
----------------------
typexpr :: { LLmlType LmlcPs }
  : function_type { $1 }

function_type :: { LLmlType LmlcPs }
  : tuple_type %prec '->' { $1 }
  | tuple_type '->' function_type { sLL $1 $3 (LmlTyArrow noExtField $1 $3) }

tuple_type :: { LLmlType LmlcPs }
  : atomic_type %prec below_DOT { $1 }
  | atomic_type lsepBy1(atomic_type, '*') { sLNE (NE.cons $1 $2) (LmlTyTuple noExtField $1 $2) }

delimited_type :: { LLmlType LmlcPs }
  : '(' function_type ')' { sLL $1 $3 (unLoc $2) }

atomic_type :: { LLmlType LmlcPs }
  : delimited_type { $1 }
  | '\'' ident { sLL $1 $2 $ LmlTyVar noExtField $2 }
  | '(' sepBy2L(function_type, ',' ) ')' typeconstr { sLL $1 $4 $ LmlTyConstr noExtField $4 $2 }
  | atomic_type typeconstr { sLL $1 $2 $ LmlTyConstr noExtField $2 [$1] }
  | typeconstr { sL1 $1 $ LmlTyConstr noExtField $1 [] }

---------------
-- Constants --
---------------
constant :: { XLocated LmlcPs (LmlLit LmlcPs) }
  : integer_literal { sL1 $1 $ LmlInt noExtField (getInt $1) }
  | char_literal { sL1 $1 $ LmlChar noExtField (getChar $1) }
  | string_literal { sL1 $1 $ LmlString noExtField (getString $1)}

--------------
-- Patterns --
--------------

pattern :: { LLmlPat LmlcPs }
  : tuple_pattern { $1 }

pattern_comma_list :: { NonEmpty (LLmlPat LmlcPs) }
  : lsepBy1Rev(simple_pattern, ',') %prec below_COMMA { NE.reverse $1 }

tuple_pattern :: { LLmlPat LmlcPs }
  : simple_pattern %prec below_COMMA { $1 }
  | simple_pattern pattern_comma_list { sLNE (NE.cons $1 $2) $ LmlPatTuple noExtField $1 $2}

delimited_pattern :: { LLmlPat LmlcPs }
  : '(' tuple_pattern ')' { sLL $1 $3 (unLoc $2) }
  | '(' tuple_pattern ':' typexpr ')' { sLL $1 $5 $ LmlPatConstraint noExtField $2 $4 }

simple_pattern :: { LLmlPat LmlcPs }
  : delimited_pattern { $1 }
  | value_name { sL1 $1 $ LmlPatVar noExtField $1 }
  | '_' { sL1 $1 $ LmlPatAny noExtField }
  | constant { sL1 $1 $ LmlPatConstant noExtField (unLoc $1) }
  | constr %prec below_DOT { sL1 $1 $ LmlPatConstruct noExtField $1 Nothing }
  | tuple_pattern '|' tuple_pattern { sLL $1 $3 $ LmlPatOr noExtField $1 $3 }
  | constr tuple_pattern %prec constr_appl { sLL $1 $2 $ LmlPatConstruct noExtField $1 (Just $2)}
  | '[' sepBy1Terminated(tuple_pattern, ';') ']' { mkListPat $1 $2 $3 }
  | tuple_pattern '::' tuple_pattern
    { let consIdent = sL1 $2 consConstruct in
      let consTuple = sLL $1 $3 $ LmlPatTuple noExtField $1 (pure $3) in
      sLL $1 $3 $ LmlPatConstruct noExtField consIdent (Just consTuple)
    }

-----------------
-- Expressions --
-----------------
expr :: { LLmlExpr LmlcPs }
  : compound_expr { $1 }

argument :: { LLmlExpr LmlcPs }
  : simple_expr { $1 }

parameter :: { LLmlPat LmlcPs }
  : pattern { $1 }

expr_comma_NE :: { NonEmpty (LLmlExpr LmlcPs) }
  : lsepBy1Rev(argument, ',') %prec below_COMMA { NE.reverse $1 }

expr_apply_NE :: { NonEmpty (LLmlExpr LmlcPs) }
  : manyNERev(simple_expr) %prec below_DOT { NE.reverse $1 }

expr_parameter_NE :: { NonEmpty (LLmlPat LmlcPs) }
  : manyNERev(parameter) %prec below_DOT { NE.reverse $1 }

type_constraint :: { LLmlType LmlcPs }
  : ':' typexpr { $2 }

compound_expr :: { LLmlExpr LmlcPs }
  : simple_expr %prec below_DOT { $1 }
  | simple_expr expr_comma_NE { sLNE (NE.cons $1 $2) $ LmlExprTuple noExtField $1 $2 }
  | constr simple_expr %prec below_DOT { sLL $1 $2 $ LmlExprConstruct noExtField $1 (Just $2) }
  | simple_expr '::' compound_expr
    { let consIdent = sL1 $2 consConstruct in
      let consTuple = sLL $1 $3 $ LmlExprTuple noExtField $1 (pure $3) in
      sLL $1 $3 $ LmlExprConstruct noExtField consIdent (Just consTuple)
    }
  | prefix_symbol compound_expr
    { let prefixIdent = sL1 $1 $ LmlExprIdent noExtField (getLongident (pure $1)) in
      sLL $1 $2 $ LmlExprApply noExtField prefixIdent (pure $2)
    }
  | '-' compound_expr %prec prec_unary_minus
    { let prefixIdent = sL1 $1 $ LmlExprIdent noExtField (mkLongident $ pure "~-") in
      sLL $1 $2 $ LmlExprApply noExtField prefixIdent (pure $2)
    }
  | simple_expr infix_op compound_expr
    { let infixIdent = sL1 $2 $ LmlExprIdent noExtField ((mkLongident . pure . unLoc) $2) in
      sLL $1 $3 $ LmlExprApply noExtField infixIdent ($1 :| [$3])
    }
  | 'if' compound_expr 'then' compound_expr 'else' compound_expr
    { sLL $1 $6 $ LmlExprIfThenElse noExtField $2 $4 $6 }
  | simple_expr expr_apply_NE { sLNE (NE.cons $1 $2) $ LmlExprApply noExtField $1 $2 }
  | 'match' compound_expr 'with' pattern_matchingNE{ sLL $1 (last $4) $ LmlExprMatch noExtField $2 $4 }
  | 'fun' expr_parameter_NE optional(type_constraint) '->' compound_expr { mkFunExpr $2 $3 $5 }
  | 'let' binding_group 'in' compound_expr { sLL $1 $4 $ LmlExprLet noExtField $2 $4 }

delimited_expr :: { LLmlExpr LmlcPs }
  : '(' compound_expr ')' { sLL $1 $3 (unLoc $2) }
  | '(' compound_expr ':' typexpr ')' { sLL $1 $5 $ LmlExprConstraint noExtField $2 $4 }

-- These exprs (except delimited_expr) can be used in application w/o parentheses
simple_expr :: { LLmlExpr LmlcPs }
  : delimited_expr { $1 }
  | value_path { sL1 $1 $ LmlExprIdent noExtField (unLoc $1) }
  | constant { sL1 $1 $ LmlExprConstant noExtField (unLoc $1) }
  | '[' sepBy1Terminated(compound_expr, ';') ']' { mkListExpr $1 $2 $3 }
  | constr %prec prec_constant_constructor { sL1 $1 $ LmlExprConstruct noExtField $1 Nothing }

when_expr :: { LLmlExpr LmlcPs }
  : 'when' expr { $2 }

pattern_matching :: { LLmlCase LmlcPs }
  : pattern optional(when_expr) '->' expr { sLL $1 $4 $ LmlCase noExtField $1 $2 $4 }

pattern_matchingNE :: { NonEmpty (LLmlCase LmlcPs) }
  : lsepBy1PreceededRev(pattern_matching, '|') %shift { NE.reverse $1 }

let_binding :: { LLmlBind LmlcPs }
  : pattern '=' expr { sLL $1 $3 $ LmlBind noExtField $1 $3 }
  | value_name expr_parameter_NE optional(type_constraint) '=' expr
    { let patIdent = sL1 $1 $ LmlPatVar noExtField $1 in
      sLL $1 $5 $ LmlBind noExtField patIdent (mkFunExpr $2 $3 $5)
    }

binding_group :: { LLmlBindGroup LmlcPs }
  : {- empty -} sepBy1(let_binding, 'and') { sLNE $1 $ LmlBindGroup noExtField NonRecursive $1 }
  | 'rec' sepBy1(let_binding, 'and') { sLL $1 (last $2) $ LmlBindGroup noExtField Recursive $2 }

----------------------
-- Type definitions --
----------------------

-- type-information rule is inlined here
typedef :: { LTyDecl LmlcPs }
  : optional(type_params) typeconstr_name {- empty -}
    { let typeParams = maybe [] toList $1 in
      sMNELL $1 $2 $2 $ DataDecl noExtField $2 typeParams []
    }
  | optional(type_params) typeconstr_name '=' type_equation
    { let typeParams = maybe [] toList $1 in
      sMNELL $1 $2 $4 $ AliasDecl noExtField $2 typeParams $4
    }
  | optional(type_params) typeconstr_name '=' type_representation
    { let typeParams = maybe [] toList $1 in
      sMNELL $1 $2 (last $4) $ DataDecl noExtField $2 typeParams (toList $4)
    }

type_equation :: { LLmlType LmlcPs }
  : typexpr { $1 }

-- For some LR related reason following code will give weird shift/reduce conflicts
-- -- type_representation :: { NonEmpty ConstructorDeclaration }
-- --   : lsepBy1Preceeded(constr_decl, '|') { $1 }
-- One below -- won't

type_representationRev :: { NonEmpty (LConDecl LmlcPs) }
  : constr_decl { pure $1 }
  | '|' constr_decl { pure $2 }
  | type_representationRev '|' constr_decl { NE.cons $3 $1 }

type_representation :: { NonEmpty (LConDecl LmlcPs) }
  : type_representationRev { NE.reverse $1 }

type_params :: { NonEmpty (LLmlType LmlcPs) }
  : type_param { pure $1 }
  | '(' sepBy1(type_param, ',') ')' { $2 }

type_param :: { LLmlType LmlcPs }
  : '\'' ident { sLL $1 $2 $ LmlTyVar noExtField $2 }

constr_args :: { NonEmpty (LLmlType LmlcPs) }
  : sepBy1(atomic_type, '*') { $1 }

of_constr_args :: { NonEmpty (LLmlType LmlcPs) }
  : 'of' constr_args { $2 }

constr_decl :: { LConDecl LmlcPs }
  : constr_name optional(of_constr_args)
    { sLMNEL $1 $2 $1 $ ConDecl noExtField $1 (maybe [] toList $2) }
  | '[' ']' optional(of_constr_args)
    { let ident = sLL $1 $2 "[]" in
      sLMNEL $1 $3 $2 $ ConDecl noExtField ident (maybe [] toList $3)
    }
  | '(' '::' ')' optional(of_constr_args)
    { let ident = sLL $1 $3 "::" in
      sLMNEL $1 $4 $3 $ ConDecl noExtField ident (maybe [] toList $4)
    }

------------------------------
-- Declarations and modules --
------------------------------
module_expression :: { LmlModule LmlcPs }
  : optional(module_definition) many(module_item)
    { LmlModule{ _lmlModExt = noExtField, _lmlModName = $1, _lmlModDecls = $2 } }

-- FIXME: Store 'module' location
module_definition :: { LLongident LmlcPs }
  : 'module' module_path { $2 }

open_declaration :: { LOpenDecl LmlcPs }
  : 'open' module_path { sLL $1 $2 $ OpenDecl noExtField $2  }

module_item :: { LLmlDecl LmlcPs }
  : open_declaration { sL1 $1 $ OpenD noExtField (unLoc $1) }
  | 'let' binding_group { sLL $1 $2 $ ValD noExtField $2 }
  | 'type' sepBy1(typedef, 'and') { sLL $1 (last $2) $ TyD noExtField $2 }

-------------
-- Helpers --
-------------
optional(p)
  : {- empty -} { Nothing }
  | p { Just $1 }

sepBy1Rev(p, sep)
  : p { pure $1 }
  | sepBy1Rev(p, sep) sep p { NE.cons $3 $1 }

sepBy1(p, sep) : sepBy1Rev(p, sep) { NE.reverse $1 }

manyRev(p)
  : {- empty -} { [] }
  | manyRev(p) p { $2 : $1 }

many(p) : manyRev(p) { reverse $1 }

manyNERev(p)
  : p { pure $1 }
  | manyNERev(p) p { NE.cons $2 $1 }

manyNE(p) : manyNERev(p) { NE.reverse $1 }

sepBy2LRev(p, sep)
  : sepBy2LRev(p, sep) sep p { $3 : $1 }
  | p sep p { [$3, $1]}

sepBy2L(p, sep) : sepBy2LRev(p, sep) { reverse $1}

lsepBy1Rev(p, sep)
  : sep p { pure $2 }
  | lsepBy1Rev(p, sep) sep p { NE.cons $3 $1 }

lsepBy1(p, sep) : lsepBy1Rev(p, sep) { NE.reverse $1 }

mkIdentRev(prefix,final)
  : final { pure $1 }
  | prefix '.' final { NE.cons $3 $1 }

mkIdent(prefix, final) : mkIdentRev(prefix,final) { NE.reverse $1 }

sepBy1Terminated(p, sep)
  : p optional(sep) { pure $1 }
  | p sep sepBy1Terminated(p, sep) { NE.cons $1 $3 }

lsepBy1PreceededRev(p, sep)
  : optional(sep) p { pure $2 }
  | lsepBy1PreceededRev(p, sep) sep p { NE.cons $3 $1 }

lsepBy1Preceeded(p, sep) : lsepBy1PreceededRev(p, sep) { NE.reverse $1 }

{
-- TODO: Find another way to print a token
parseError :: LToken -> Alex a
parseError token = do
  (AlexPn _ line column, _, _, str) <- alexGetInput
  alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ (show (unLoc token))

lexer :: (LToken -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

getIdent :: LToken -> Text
getIdent (L _ (TokIdent _ val)) = val
getIdent (L _ (TokInfixSymbol0 val)) = val
getIdent (L _ (TokInfixSymbol1 val)) = val
getIdent (L _ (TokInfixSymbol2 val)) = val
getIdent (L _ (TokInfixSymbol3 val)) = val
getIdent (L _ (TokInfixSymbol4 val)) = val
getIdent (L _ TokStar) = "*"
getIdent (L _ TokPlus) = "+"
getIdent (L _ TokMinus) = "-"
getIdent (L _ TokEq) = "="
getIdent (L _ TokDoubleBar) = "||"
getIdent (L _ TokBoolAnd) = "&&"
getIdent (L _ TokMod) = "mod"
getIdent (L _ TokLand) = "land"
getIdent (L _ TokLor) = "lor"
getIdent (L _ TokLxor) = "lxor"
getIdent (L _ TokLsl) = "lsl"
getIdent (L _ TokLsr) = "lsr"
getIdent (L _ TokAsr) = "asr"
getIdent (L _ (TokPrefixSymbol val)) = val
-- getIdent (L _ TokBar) = "|"

getLongident :: NonEmpty LToken -> Longident
getLongident = mkLongident . fmap getIdent

getInt :: LToken -> Int
getInt (L _ (TokInt val)) = val

getChar :: LToken -> Char
getChar (L _ (TokChar val)) = val

getString :: LToken -> Text
getString (L _ (TokString val)) = val

nilConstruct :: Longident
nilConstruct = (mkLongident . pure) "[]"

consConstruct :: Longident
consConstruct = (mkLongident . pure) "::"

unitConstruct :: Longident
unitConstruct = (mkLongident . pure) "()"

-- Combining helpers
{-# INLINE comb2 #-}
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

-- | Strict 'GenLocated' constructor
{-# INLINE sL #-}
sL :: l -> e -> GenLocated l e
sL loc e = loc `seq` e `seq` L loc e

-- | Combine first and last locations from 'NonEmpty'
{-# INLINE sLNE #-}
sLNE :: NonEmpty (Located a) -> b -> Located b
sLNE ne = sL (combineLocs (head ne) (last ne))

-- | Combine two locations
{-# INLINE sLL #-}
sLL :: Located a -> Located b -> c -> Located c
sLL a b = sL (comb2 a b)

-- | Repack 'Located'
{-# INLINE sL1 #-}
sL1 :: Located a -> b -> Located b
sL1 a = sL (getLoc a)

-- | Combine two location when first is @'Maybe' ('NonEmpty' a)@.
-- If first is 'Nothing', then second one is selected.
-- Third is always used as the last.
{-# INLINE sMNELL #-}
sMNELL :: Maybe (NonEmpty (Located a)) -> Located b -> Located c -> d -> Located d
sMNELL a b c = sL outLoc
 where
  leftLoc :: SrcSpan
  leftLoc = maybe (getLoc b) (getLoc . head) a
  outLoc :: SrcSpan
  outLoc = combineSrcSpans leftLoc (getLoc c)

-- | Combine two location when second is @'Maybe' ('NonEmpty' a)@.
-- If second is 'Nothing', then third one is selected.
-- First is always used as the leader.
{-# INLINE sLMNEL #-}
sLMNEL ::  Located a -> Maybe (NonEmpty (Located b)) -> Located c -> d -> Located d
sLMNEL a b c = sL outLoc
 where
  rightLoc :: SrcSpan
  rightLoc = maybe (getLoc c) (getLoc . last) b
  outLoc :: SrcSpan
  outLoc = combineSrcSpans (getLoc a) rightLoc

mkListPat :: LToken -> NonEmpty (LLmlPat LmlcPs) -> LToken -> LLmlPat LmlcPs
mkListPat lBracket list rBracket = foldr helper init list
 where
  init :: LLmlPat LmlcPs
  init = sL1 rBracket $ LmlPatConstruct noExtField (sL generatedSrcSpan nilConstruct) Nothing
  helper :: LLmlPat LmlcPs -> LLmlPat LmlcPs -> LLmlPat LmlcPs
  helper x acc = sLL x rBracket $ LmlPatConstruct noExtField (sL generatedSrcSpan consConstruct) (Just pat)
   where
    pat = sLL x acc $ LmlPatTuple noExtField x (pure acc)

mkListExpr :: LToken -> NonEmpty (LLmlExpr LmlcPs) -> LToken -> LLmlExpr LmlcPs
mkListExpr lBracket list rBracket = foldr helper init list
 where
  init :: LLmlExpr LmlcPs
  init = sL1 rBracket $ LmlExprConstruct noExtField (sL generatedSrcSpan nilConstruct) Nothing
  helper :: LLmlExpr LmlcPs -> LLmlExpr LmlcPs -> LLmlExpr LmlcPs
  helper x acc = sLL x rBracket $ LmlExprConstruct noExtField (sL generatedSrcSpan consConstruct) (Just expr)
   where
    expr = sLL x acc $ LmlExprTuple noExtField x (pure acc)

mkFunExpr :: NonEmpty (LLmlPat LmlcPs) -> Maybe (LLmlType LmlcPs) -> LLmlExpr LmlcPs -> LLmlExpr LmlcPs
mkFunExpr pats mType rhsExpr = foldr helper init pats
 where
  init :: LLmlExpr LmlcPs
  init = case mType of
           Just typ -> sLL typ rhsExpr $ LmlExprConstraint noExtField rhsExpr typ
           Nothing -> rhsExpr
  helper :: LLmlPat LmlcPs -> LLmlExpr LmlcPs -> LLmlExpr LmlcPs
  helper pat acc = sLL pat acc $ LmlExprFunction noExtField pat acc

-- | Parser entry point
parseLamagraphML :: Text -> Either String (LmlModule LmlcPs)
parseLamagraphML text = runAlex text pLamagraphML
}
