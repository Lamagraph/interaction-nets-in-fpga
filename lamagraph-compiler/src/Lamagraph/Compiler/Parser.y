{
{- I have no clue whether this make a difference in *any* speed,
but Happy docs recommend this flag -}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Lamagraph.Compiler.Parser (parseLamagraphML) where

import Relude
-- {- We can use partial functions, because of the way parsing works -}
-- import Relude.Unsafe (fromJust)

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
  int32_literal { L _ (TokInt32 _) }
  uint32_literal { L _ (TokUInt32 _) }
  int64_literal { L _ (TokInt64 _) }
  uint64_literal { L _ (TokUInt64 _) }
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
          int32_literal uint32_literal int64_literal uint64_literal
          char_literal string_literal 'false' '[' ']' prefix_symbol
          'true' '_'

%%
-- Happy does pattern-matching againt %token directives, thus we need a rule for ident
-- TODO: RdrName?
ident :: { XLocated LmlcPs Text }
  : lowercase_ident { sL1 $1 $ getIdent $1 }
  | capitalized_ident { sL1 $1 $ getIdent $1 }

---------------
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

constr_name :: { LToken } : capitalized_ident { $1 }

typeconstr_name :: { LToken }
  : lowercase_ident { $1 }

---------------------
-- Qualified names --
---------------------
-- value_path :: { NonEmpty Token }
--   : mkIdent(module_pathT, lowercase_ident) { $1 }

constr :: { LLongident LmlcPs }
  : mkIdent(module_pathT, capitalized_ident) { sLNE $1 $ getLongident $1  }
  | '[' ']' { sLL $1 $2 nilConstruct }
  | '(' ')' { sLL $1 $2 unitConstruct }
  | 'true' { sL1 $1 $ (mkLongident . pure) "true" }
  | 'false' { sL1 $1 $ (mkLongident . pure) "false" }

typeconstr :: { LLongident LmlcPs }
  : mkIdent(module_pathT, lowercase_ident) { sLNE $1 $ getLongident $1 }

module_pathT :: { NonEmpty LToken }
  : mkIdent(module_pathT, capitalized_ident) { $1 }

module_path :: { LLongident LmlcPs }
  : module_pathT { sLNE $1 $ getLongident $1 }

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
  | int32_literal { sL1 $1 $ LmlInt32 noExtField (getInt32 $1) }
  | uint32_literal { sL1 $1 $ LmlUInt32 noExtField (getUInt32 $1) }
  | int64_literal { sL1 $1 $ LmlInt64 noExtField (getInt64 $1) }
  | uint64_literal { sL1 $1 $ LmlUInt64 noExtField (getUInt64 $1) }
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
  | simple_pattern pattern_comma_list
    { sLNE (NE.cons $1 $2) $ LmlPatTuple noExtField $1 $2}

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
-- expr :: { Expression }
--   : compound_expr { $1 }

-- argument :: { Expression }
--   : simple_expr { $1 }

-- parameter :: { Pattern }
--   : pattern { $1 }

-- expr_comma_NE :: { NonEmpty Expression }
--   : lsepBy1Rev(argument, ',') %prec below_COMMA { NE.reverse $1 }

-- expr_apply_NE :: { NonEmpty Expression }
--   : manyNERev(simple_expr) %prec below_DOT { NE.reverse $1 }

-- expr_parameter_NE :: { NonEmpty Pattern }
--   : manyNERev(parameter) %prec below_DOT { NE.reverse $1 }

-- type_constraint :: { CoreType }
--   : ':' typexpr { $2 }

-- compound_expr :: { Expression }
--   : simple_expr %prec below_DOT { $1 }
--   | simple_expr expr_comma_NE
--     { Expression{ _pExpDesc = PExpTuple $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }
--   | constr simple_expr %prec below_DOT
--     { Expression{ _pExpDesc = PExpConstruct (getLongident $1) (Just $2), _pExpLoc = (head $1) ^. loc <~> $2 ^. pExpLoc } }
--   | simple_expr '::' compound_expr
--     { Expression{ _pExpDesc = PExpConstruct ("::")
--                     (Just $ Expression{ _pExpDesc = PExpTuple $1 ($3)
--                                       , _pExpLoc = $1 ^. pExpLoc <~> $3 ^. pExpLoc })
--                 , _pExpLoc = $2 <-> $2 }
--     }
--   | prefix_symbol compound_expr
--     { let ident = Expression{ _pExpDesc = PExpIdent ($ fromJust $ $1 ^? tokenType . _TokPrefixSymbol)
--                             , _pExpLoc = $1 <-> $1 } in
--       Expression{ _pExpDesc = PExpApply ident ($2), _pExpLoc = $1 ^. loc <~> $2 ^. pExpLoc }
--     }
--   | '-' compound_expr %prec prec_unary_minus
--     { let um = Expression{ _pExpDesc = PExpIdent ("~-"), _pExpLoc = $1 <-> $1 } in
--       Expression{ _pExpDesc = PExpApply um ($2), _pExpLoc = $1 ^. loc <~> $2 ^. pExpLoc  }
--     }
--   | simple_expr infix_op compound_expr
--     { let ident = Expression{ _pExpDesc = PExpIdent (getInfixIdent $2), _pExpLoc = $2 <-> $2 } in
--       Expression{ _pExpDesc = PExpApply ident ($1 :| [$3]), _pExpLoc = $1 ^. pExpLoc <~> $3 ^. pExpLoc }
--     }
--   | 'if' compound_expr 'then' compound_expr 'else' compound_expr
--     { Expression{ _pExpDesc = PExpIfThenElse $2 $4 $6, _pExpLoc = $1 ^. loc <~> $6 ^. pExpLoc  } }
--   | simple_expr expr_apply_NE { Expression{ _pExpDesc = PExpApply $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }
--   | 'match' compound_expr 'with' pattern_matchingNE
--     { Expression{ _pExpDesc = PExpMatch $2 $4, _pExpLoc = $1 ^. loc <~> (last $4) ^. pCRhs . pExpLoc } }
--   | 'fun' expr_parameter_NE optional(type_constraint) '->' compound_expr
--     { mkFunExpr $2 $3 $5 }
--   | 'let' rec sepBy1(let_binding, 'and') 'in' compound_expr
--     { Expression{ _pExpDesc = PExpLet $2 $3 $5, _pExpLoc = $1 ^. loc <~> $5 ^. pExpLoc } }

-- delimited_expr :: { Expression }
--   : '(' compound_expr ')' { $2{ _pExpLoc = $1 <-> $3} }
--   | '(' compound_expr ':' typexpr ')'
--     { Expression{ _pExpDesc = PExpConstraint $2 $4, _pExpLoc = $1 <-> $5 } }

-- -- These exprs (except delimited_expr) can be used in application w/o parentheses
-- simple_expr :: { Expression }
--   : delimited_expr { $1 }
--   | value_path
--     { Expression{ _pExpDesc = PExpIdent $ getLongident $1, _pExpLoc = head $1 <-> last $1 } }
--   | constant
--     { Expression{ _pExpDesc = PExpConstant $1, _pExpLoc = $1 ^. pConstLoc <~> $1 ^. pConstLoc } }
--   | '[' ']' { Expression{ _pExpDesc = PExpConstruct ("[]") Nothing, _pExpLoc = $1 <-> $2 } }
--   | '(' ')' { Expression{ _pExpDesc = PExpConstruct ("()") Nothing, _pExpLoc = $1 <-> $2 } }
--   | 'true' { Expression{ _pExpDesc = PExpConstruct ("true") Nothing, _pExpLoc = $1 <-> $1 } }
--   | 'false' { Expression{ _pExpDesc = PExpConstruct ("false") Nothing, _pExpLoc = $1 <-> $1 } }
--   | '[' sepBy1Terminated(compound_expr, ';') ']' { mkListExpr $1 $2 $3 }
--   | constr %prec prec_constant_constructor
--     { Expression{ _pExpDesc = PExpConstruct (getLongident $1) Nothing
--                 , _pExpLoc = (head $1) <-> (last $1) }
--     }

-- when_expr :: { Expression }
--   : 'when' expr { $2 }

-- pattern_matching :: { Case }
--   : pattern optional(when_expr) '->' expr
--     { Case{ _pCLhs = $1, _pCGuard = $2, _pCRhs = $4 } }

-- pattern_matchingNE :: { NonEmpty Case }
--   : lsepBy1PreceededRev(pattern_matching, '|') %shift { NE.reverse $1 }

-- let_binding :: { ValueBinding }
--   : pattern '=' expr
--     { ValueBinding{ _pVBPat = $1
--                   , _pVBExpr = $3
--                   , _pVBLoc = $1 ^. pPatLoc <~> $3 ^. pExpLoc }
--     }
--   | value_name expr_parameter_NE optional(type_constraint) '=' expr
--     { let namePat = Pattern{ _pPatDesc = PPatVar $ getIdent $1, _pPatLoc = $1 <-> $1 } in
--       ValueBinding{ _pVBPat = namePat
--                   , _pVBExpr = mkFunExpr $2 $3 $5
--                   , _pVBLoc = $1 ^. loc <~> $5 ^. pExpLoc }
--     }

-- Type definitions

-- type_definition :: { }
--   : {undefined}
  -- : 'type' sepBy1(typedef, 'and') {}
-- typedef :: { TypeDeclaration }
--   : optional(type_params) typeconstr_name type_information
--     { TypeDeclaration{ _pTypeName = getIdent $2
--                      , _pTypeParams = maybe [] toList $1
--                      , _pTypeKind = $3
--                      , _pTypeLoc = (maybe ($2 ^. loc) ((view pTypLoc) . last) $1) <~> $2 ^. loc } -- FIXME: type_informatiom MUST have location
--     }

-- type_information :: { TypeKind }
--   : {- empty -} { PTypeAbstract Nothing }
--   | '=' type_equation { PTypeAbstract (Just $2) }
--   | '=' type_representation { PTypeVariant $2 }

-- type_equation :: { CoreType }
--   : typexpr { $1 }

-- type_representationRev :: { [ConstructorDeclaration] }
--   : constr_decl { [$1] }
--   | '|' constr_decl { [$2] }
--   | type_representationRev '|' constr_decl { $3 : $1 }

-- type_representation :: { [ConstructorDeclaration] }
--   : '|' { [] }
--   | type_representationRev { $1 }

-- -- For some LR related reason following code will give shift/reduce conflicts
-- -- -- type_representation :: { NonEmpty ConstructorDeclaration }
-- -- --   : lsepBy1Preceeded(constr_decl, '|') { $1 }
-- -- One above -- won't

-- type_params :: { NonEmpty CoreType }
--   : type_param { pure $1 }
--   | '(' sepBy1(type_param, ',') ')' { $2 }

-- type_param :: { CoreType }
--   : '\'' ident { CoreType{ _pTyp = PTypVar $ getIdent $2, _pTypLoc = $1 <-> $2 } }

-- of_constr_args :: { NonEmpty CoreType }
--   : 'of' constr_args { $2 }

-- constr_decl :: { ConstructorDeclaration }
--   :
--   constr_name optional(of_constr_args)
--     { ConstructorDeclaration{ _pCDName = getIdent $1
--                             , _pCDArgs = maybe [] toList $2
--                             , _pCDLoc = $1 ^. loc <~> maybe ($1 ^. loc) ((view pTypLoc) . last) $2 }
--     }
--   |
--   '[' ']' optional(of_constr_args)
--     { ConstructorDeclaration{ _pCDName = "[]"
--                             , _pCDArgs = maybe [] toList $3
--                             , _pCDLoc = $1 ^. loc <~> maybe ($2 ^. loc) ((view pTypLoc) . last) $3 }
--     }
--   | '(' '::' ')' optional(of_constr_args)
--     { ConstructorDeclaration{ _pCDName = "::"
--                             , _pCDArgs = maybe [] toList $4
--                             , _pCDLoc = $1 ^. loc <~> maybe ($3 ^. loc) ((view pTypLoc) . last) $4 }
--     }

-- constr_args :: { NonEmpty CoreType }
--   : sepBy1(atomic_type, '*') { $1 }

-- Declarations and modules
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
  -- | 'let' rec sepBy1(let_binding, 'and') { PStrValue $2 $3 }
  -- | type_definition { PStrType $2 }

-- Helpers
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

{- rec :: { RecFlag }
rec
  : {- empty -} { NonRecursive }
  | 'rec' { Recursive } -}

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

getInt32 :: LToken -> Int32
getInt32 (L _ (TokInt32 val)) = val

getUInt32 :: LToken -> Word32
getUInt32 (L _ (TokUInt32 val)) = val

getInt64 :: LToken -> Int64
getInt64 (L _ (TokInt64 val)) = val

getUInt64 :: LToken -> Word64
getUInt64 (L _ (TokUInt64 val)) = val

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
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

-- | Strict 'GenLocated' constructor
sL :: l -> e -> GenLocated l e
sL loc e = loc `seq` e `seq` L loc e

-- | Combine first and last locations from 'NonEmpty'
sLNE :: NonEmpty (Located a) -> b -> Located b
sLNE ne = sL (combineLocs (head ne) (last ne))

-- | Combine two locations
sLL :: Located a -> Located b -> c -> Located c
sLL a b = sL (comb2 a b)

-- | Repack 'Located'
sL1 :: Located a -> b -> Located b
sL1 a = sL (getLoc a)

mkListPat :: LToken -> NonEmpty (LLmlPat LmlcPs) -> LToken -> LLmlPat LmlcPs
mkListPat lBracket list rBracket = foldr helper init list
 where
  init :: LLmlPat LmlcPs
  init = sL1 rBracket $ LmlPatConstruct noExtField (sL generatedSrcSpan nilConstruct) Nothing
  helper :: LLmlPat LmlcPs -> LLmlPat LmlcPs -> LLmlPat LmlcPs
  helper x acc = sLL x rBracket $ LmlPatConstruct noExtField (sL generatedSrcSpan consConstruct) (Just pat)
   where
    pat = sLL x acc $ LmlPatTuple noExtField x (pure acc)

-- mkListExpr :: Token -> NonEmpty Expression -> Token -> Expression
-- mkListExpr lBracket list rBracket = foldr helper init list
--  where
--   init :: Expression
--   init = Expression{ _pExpDesc = PExpConstruct ("[]") Nothing, _pExpLoc = rBracket <-> rBracket }
--   helper :: Expression -> Expression -> Expression
--   helper x acc = Expression
--     { _pExpDesc = PExpConstruct ("::") (Just expr)
--     , _pExpLoc = x ^. pExpLoc <~> rBracket ^. loc
--     }
--    where
--     expr = Expression{ _pExpDesc = PExpTuple x (acc), _pExpLoc = x ^. pExpLoc <~> acc ^. pExpLoc }

-- mkFunExpr :: NonEmpty Pattern -> Maybe CoreType -> Expression -> Expression
-- mkFunExpr pats mType rhsExpr = foldr helper init pats
--  where
--   init :: Expression
--   init = case mType of
--            Just typ -> Expression{ _pExpDesc = PExpConstraint rhsExpr typ
--                                   , _pExpLoc = typ ^. pTypLoc <~> rhsExpr ^. pExpLoc
--                                   }
--            Nothing -> rhsExpr
--   helper :: Pattern -> Expression -> Expression
--   helper pat acc = Expression{ _pExpDesc = PExpFunction pat acc, _pExpLoc = pat ^. pPatLoc <~> acc ^. pExpLoc }

parseLamagraphML :: Text -> Either String (LmlModule LmlcPs)
parseLamagraphML text = runAlex text pLamagraphML
}
