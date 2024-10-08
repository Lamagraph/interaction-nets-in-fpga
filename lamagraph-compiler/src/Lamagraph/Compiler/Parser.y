{
{- I have no clue whether this make a difference in *any* speed,
but Happy docs recommend this flag -}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Lamagraph.Compiler.Parser (parseLamagraphML, (<~>), (<->)) where

import Relude
{- We can use partial functions, because of the way parsing works -}
import Relude.Unsafe (fromJust)

import Control.Lens
import qualified Data.List.NonEmpty.Extra as NE
-- Required for Happy code
import qualified Prelude

import Lamagraph.Compiler.Parser.Lexer
import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.ParseTree
}

%name pLamagraphML module_expression
%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { Token{ _tokenType = TokEOF } }

%token
  -- Identifiers
  capitalized_ident { Token{ _tokenType = TokIdent Capitalized _ } }
  lowercase_ident { Token{ _tokenType = TokIdent Lowercase _ } }
  -- Integer literals
  integer_literal { Token{ _tokenType = TokInt _ } }
  int32_literal { Token{ _tokenType = TokInt32 _ } }
  uint32_literal { Token{ _tokenType = TokUInt32 _ } }
  int64_literal { Token{ _tokenType = TokInt64 _ } }
  uint64_literal { Token{ _tokenType = TokUInt64 _ } }
  -- Character liteal
  char_literal { Token{ _tokenType = TokChar _ } }
  -- String literal
  string_literal { Token{ _tokenType = TokString _ } }
  -- Operators
  infix_symbol0 { Token{ _tokenType = TokInfixSymbol0 _ } }
  infix_symbol1 { Token{ _tokenType = TokInfixSymbol1 _ } }
  infix_symbol2 { Token{ _tokenType = TokInfixSymbol2 _ } }
  infix_symbol3 { Token{ _tokenType = TokInfixSymbol3 _ } }
  infix_symbol4 { Token{ _tokenType = TokInfixSymbol4 _ } }
  prefix_symbol { Token{ _tokenType = TokPrefixSymbol _ } }
  -- Keywords
  'and' { Token{ _tokenType = TokAnd } }
  'asr' { Token{ _tokenType = TokAsr } }
  'else' { Token{ _tokenType = TokElse } }
  'false' { Token{ _tokenType = TokFalse } }
  'fun' { Token{ _tokenType = TokFun } }
  'if' { Token{ _tokenType = TokIf } }
  'in' { Token{ _tokenType = TokIn } }
  'land' { Token{ _tokenType = TokLand } }
  'let' { Token{ _tokenType = TokLet } }
  'lor' { Token{ _tokenType = TokLor } }
  'lsl' { Token{ _tokenType = TokLsl } }
  'lsr' { Token{ _tokenType = TokLsr } }
  'lxor' { Token{ _tokenType = TokLxor } }
  'match' { Token{ _tokenType = TokMatch } }
  'mod' { Token{ _tokenType = TokMod } }
  'module' { Token{ _tokenType = TokModule } }
  'of' { Token{ _tokenType = TokOf } }
  'open' { Token{ _tokenType = TokOpen } }
  'rec' { Token{ _tokenType = TokRec } }
  'then' { Token{ _tokenType = TokThen } }
  'true' { Token{ _tokenType = TokTrue } }
  'type' { Token{ _tokenType = TokType } }
  'when' { Token{ _tokenType = TokWhen } }
  'with' { Token{ _tokenType = TokWith } }
  '&&' { Token{ _tokenType = TokBoolAnd } }
  '\'' { Token{ _tokenType = TokApostrophe } }
  '(' { Token{ _tokenType = TokLeftPar } }
  ')' { Token{ _tokenType = TokRightPar } }
  '*' { Token{ _tokenType = TokStar } }
  '+' { Token{ _tokenType = TokPlus } }
  ',' { Token{ _tokenType = TokComma } }
  '-' { Token{ _tokenType = TokMinus } }
  '->' { Token{ _tokenType = TokArrow } }
  ':' { Token{ _tokenType = TokColon } }
  '::' { Token{ _tokenType = TokDoubleColon } }
  ';' { Token{ _tokenType = TokSemicolon } }
  '=' { Token{ _tokenType = TokEq } }
  '[' { Token{ _tokenType = TokLeftBracket } }
  ']' { Token{ _tokenType = TokRightBracket } }
  '_' { Token{ _tokenType = TokWildcard } }
  '.' { Token{ _tokenType = TokDot } }
  '|' { Token{ _tokenType = TokBar } }
  '||' { Token{ _tokenType = TokDoubleBar } }

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
ident :: { Token }
  : lowercase_ident { $1 }
  | capitalized_ident { $1 }

-- Basic names
value_name :: { Token }
  : lowercase_ident { $1 }
  | '(' operator_name ')' { $2 }

operator_name :: { Token }
  : prefix_symbol { $1 }
  | infix_op { $1 }

infix_op :: { Token }
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

constr_name :: { Token } : capitalized_ident { $1 }

typeconstr_name :: { Token }
  : lowercase_ident { $1 }

-- Qualified names
value_path :: { NonEmpty Token }
  : mkIdent(module_path, lowercase_ident) { $1 }

constr :: { NonEmpty Token }
  {- It would be ideal to have @true@, @false@, @()@, @[]@ here,
    but it breaks longident logic -}
  : mkIdent(module_path, capitalized_ident) { $1 }

typeconstr :: { NonEmpty Token }
  : mkIdent(module_path, lowercase_ident) { $1 }

module_path :: { NonEmpty Token }
  : mkIdent(module_path, capitalized_ident) { $1 }

-- Type expressions
typexpr :: { CoreType }
  : function_type { $1 }

function_type :: { CoreType }
  : tuple_type %prec '->' { $1 }
  | tuple_type '->' function_type
    { CoreType { _pTyp = PTypArrow $1 $3, _pTypLoc = $1 ^. pTypLoc <~> $3 ^. pTypLoc } }

tuple_type :: { CoreType }
  : atomic_type %prec below_DOT { $1 }
  | atomic_type lsepBy1(atomic_type, '*')
    { CoreType{ _pTyp = PTypTuple $1 $2, _pTypLoc = $1 ^. pTypLoc <~> (last $2) ^. pTypLoc } }

delimited_type :: { CoreType }
  : '(' function_type ')' { $2{ _pTypLoc = $1 <-> $3 } }

atomic_type :: { CoreType }
  : delimited_type { $1 }
  | '\'' ident { CoreType{ _pTyp = PTypVar $ getIdent $2, _pTypLoc = $1 <-> $2 } }
  | '(' sepBy2L(function_type, ',' ) ')' typeconstr
    { CoreType { _pTyp = PTypConstr (getLongident $4) $2, _pTypLoc = $1 ^. loc <~> (last $4) ^. loc } }
  | atomic_type typeconstr
    { CoreType { _pTyp = PTypConstr (getLongident $2) [$1], _pTypLoc = $1 ^. pTypLoc <~> (last $2) ^. loc } }
  | typeconstr
    { CoreType { _pTyp = PTypConstr (getLongident $1) [], _pTypLoc = head $1 <-> last $1 } }

-- Constants
constant :: { Constant }
  : integer_literal
    { Constant{ _pConstDesc = PConstInt $ fromJust $ $1 ^? tokenType . _TokInt, _pConstLoc = $1 <-> $1 } }
  | int32_literal
    { Constant{ _pConstDesc = PConstInt32 $ fromJust $ $1 ^? tokenType . _TokInt32, _pConstLoc = $1 <-> $1 } }
  | uint32_literal
    { Constant{ _pConstDesc = PConstUInt32 $ fromJust $ $1 ^? tokenType . _TokUInt32, _pConstLoc = $1 <-> $1 } }
  | int64_literal
    { Constant{ _pConstDesc = PConstInt64 $ fromJust $ $1 ^? tokenType . _TokInt64, _pConstLoc = $1 <-> $1 } }
  | uint64_literal
    { Constant{ _pConstDesc = PConstUInt64 $ fromJust $ $1 ^? tokenType . _TokUInt64, _pConstLoc = $1 <-> $1 } }
  | char_literal
    { Constant{ _pConstDesc = PConstChar $ fromJust $ $1 ^? tokenType . _TokChar, _pConstLoc = $1 <-> $1 } }
  | string_literal
    { Constant{ _pConstDesc = PConstString $ fromJust $ $1 ^? tokenType . _TokString, _pConstLoc = $1 <-> $1 } }

-- Patterns

pattern :: { Pattern }
  : tuple_pattern { $1 }

pattern_comma_list : lsepBy1Rev(simple_pattern, ',') %prec below_COMMA { NE.reverse $1 }

tuple_pattern :: { Pattern }
  : simple_pattern %prec below_COMMA { $1 }
  | simple_pattern pattern_comma_list
  { Pattern{ _pPatDesc = PPatTuple $1 $2, _pPatLoc = $1 ^. pPatLoc <~> (last $2) ^. pPatLoc } }

delimited_pattern :: { Pattern }
  : '(' tuple_pattern ')' { $2{ _pPatLoc = $1 <-> $3 } }
  | '(' tuple_pattern ':' typexpr ')'
    { Pattern{ _pPatDesc = PPatConstraint $2 $4, _pPatLoc = $1 <-> $5 } }

simple_pattern :: { Pattern }
  : delimited_pattern { $1 }
  | value_name { Pattern{ _pPatDesc = PPatVar $ getIdent $1, _pPatLoc = $1 <-> $1 } }
  | '_' { Pattern{ _pPatDesc = PPatAny, _pPatLoc = $1 <-> $1 } }
  | constant { Pattern{ _pPatDesc = PPatConstant $1, _pPatLoc = $1 ^. pConstLoc <~> $1 ^. pConstLoc } }
  | constr %prec below_DOT
    { Pattern{ _pPatDesc = PPatConstruct (getLongident $1) Nothing, _pPatLoc = (head $1) <-> (last $1) } }
  | '[' ']' { Pattern{ _pPatDesc = PPatConstruct (pure "[]") Nothing, _pPatLoc = $1 <-> $2 } }
  | '(' ')' { Pattern{ _pPatDesc = PPatConstruct (pure "()") Nothing, _pPatLoc = $1 <-> $2 } }
  | 'true' { Pattern{ _pPatDesc = PPatConstruct (pure "true") Nothing, _pPatLoc = $1 <-> $1 } }
  | 'false' { Pattern{ _pPatDesc = PPatConstruct (pure "false") Nothing, _pPatLoc = $1 <-> $1 } }
  | tuple_pattern '|' tuple_pattern { Pattern{ _pPatDesc = PPatOr $1 $3, _pPatLoc = $1 ^. pPatLoc <~> $3 ^. pPatLoc } }
  | constr tuple_pattern %prec constr_appl
    { Pattern{ _pPatDesc = PPatConstruct (getLongident $1) (Just $2)
             , _pPatLoc = (head $1) ^. loc <~> $2 ^. pPatLoc } }
  | '[' sepBy1Terminated(tuple_pattern, ';') ']' { mkListPat $1 $2 $3 }
  | tuple_pattern '::' tuple_pattern
    { Pattern{ _pPatDesc = PPatConstruct (pure "::")
                (Just $ Pattern{ _pPatDesc = PPatTuple $1 (pure $3)
                               , _pPatLoc = $1 ^. pPatLoc <~> $3 ^. pPatLoc })
             , _pPatLoc = $2 <-> $2 }
    }

-- Expressions
expr :: { Expression }
  : compound_expr { $1 }

argument :: { Expression }
  : simple_expr { $1 }

parameter :: { Pattern }
  : pattern { $1 }

expr_comma_NE :: { NonEmpty Expression }
  : lsepBy1Rev(argument, ',') %prec below_COMMA { NE.reverse $1 }

expr_apply_NE :: { NonEmpty Expression }
  : manyNERev(simple_expr) %prec below_DOT { NE.reverse $1 }

expr_parameter_NE :: { NonEmpty Pattern }
  : manyNERev(parameter) %prec below_DOT { NE.reverse $1 }

type_constraint :: { CoreType }
  : ':' typexpr { $2 }

compound_expr :: { Expression }
  : simple_expr %prec below_DOT { $1 }
  | simple_expr expr_comma_NE
    { Expression{ _pExpDesc = PExpTuple $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }
  | constr simple_expr %prec below_DOT
    { Expression{ _pExpDesc = PExpConstruct (getLongident $1) (Just $2), _pExpLoc = (head $1) ^. loc <~> $2 ^. pExpLoc } }
  | simple_expr '::' compound_expr
    { Expression{ _pExpDesc = PExpConstruct (pure "::")
                    (Just $ Expression{ _pExpDesc = PExpTuple $1 (pure $3)
                                      , _pExpLoc = $1 ^. pExpLoc <~> $3 ^. pExpLoc })
                , _pExpLoc = $2 <-> $2 }
    }
  | prefix_symbol compound_expr
    { let ident = Expression{ _pExpDesc = PExpIdent (pure $ fromJust $ $1 ^? tokenType . _TokPrefixSymbol)
                            , _pExpLoc = $1 <-> $1 } in
      Expression{ _pExpDesc = PExpApply ident (pure $2), _pExpLoc = $1 ^. loc <~> $2 ^. pExpLoc }
    }
  | '-' compound_expr %prec prec_unary_minus
    { let um = Expression{ _pExpDesc = PExpIdent (pure "~-"), _pExpLoc = $1 <-> $1 } in
      Expression{ _pExpDesc = PExpApply um (pure $2), _pExpLoc = $1 ^. loc <~> $2 ^. pExpLoc  }
    }
  | simple_expr infix_op compound_expr
    { let ident = Expression{ _pExpDesc = PExpIdent (getInfixIdent $2), _pExpLoc = $2 <-> $2 } in
      Expression{ _pExpDesc = PExpApply ident ($1 :| [$3]), _pExpLoc = $1 ^. pExpLoc <~> $3 ^. pExpLoc }
    }
  | 'if' compound_expr 'then' compound_expr 'else' compound_expr
    { Expression{ _pExpDesc = PExpIfThenElse $2 $4 $6, _pExpLoc = $1 ^. loc <~> $6 ^. pExpLoc  } }
  | simple_expr expr_apply_NE { Expression{ _pExpDesc = PExpApply $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }
  | 'match' compound_expr 'with' pattern_matchingNE
    { Expression{ _pExpDesc = PExpMatch $2 $4, _pExpLoc = $1 ^. loc <~> (last $4) ^. pCRhs . pExpLoc } }
  | 'fun' expr_parameter_NE optional(type_constraint) '->' compound_expr
    { mkFunExpr $2 $3 $5 }
  | 'let' rec sepBy1(let_binding, 'and') 'in' compound_expr
    { Expression{ _pExpDesc = PExpLet $2 $3 $5, _pExpLoc = $1 ^. loc <~> $5 ^. pExpLoc } }

delimited_expr :: { Expression }
  : '(' compound_expr ')' { $2{ _pExpLoc = $1 <-> $3} }
  | '(' compound_expr ':' typexpr ')'
    { Expression{ _pExpDesc = PExpConstraint $2 $4, _pExpLoc = $1 <-> $5 } }

-- These exprs (except delimited_expr) can be used in application w/o parentheses
simple_expr :: { Expression }
  : delimited_expr { $1 }
  | value_path
    { Expression{ _pExpDesc = PExpIdent $ getLongident $1, _pExpLoc = head $1 <-> last $1 } }
  | constant
    { Expression{ _pExpDesc = PExpConstant $1, _pExpLoc = $1 ^. pConstLoc <~> $1 ^. pConstLoc } }
  | '[' ']' { Expression{ _pExpDesc = PExpConstruct (pure "[]") Nothing, _pExpLoc = $1 <-> $2 } }
  | '(' ')' { Expression{ _pExpDesc = PExpConstruct (pure "()") Nothing, _pExpLoc = $1 <-> $2 } }
  | 'true' { Expression{ _pExpDesc = PExpConstruct (pure "true") Nothing, _pExpLoc = $1 <-> $1 } }
  | 'false' { Expression{ _pExpDesc = PExpConstruct (pure "false") Nothing, _pExpLoc = $1 <-> $1 } }
  | '[' sepBy1Terminated(compound_expr, ';') ']' { mkListExpr $1 $2 $3 }
  | constr %prec prec_constant_constructor
    { Expression{ _pExpDesc = PExpConstruct (getLongident $1) Nothing
                , _pExpLoc = (head $1) <-> (last $1) }
    }

when_expr :: { Expression }
  : 'when' expr { $2 }

pattern_matching :: { Case }
  : pattern optional(when_expr) '->' expr
    { Case{ _pCLhs = $1, _pCGuard = $2, _pCRhs = $4 } }

pattern_matchingNE :: { NonEmpty Case }
  : lsepBy1PreceededRev(pattern_matching, '|') %shift { NE.reverse $1 }

let_binding :: { ValueBinding }
  : pattern '=' expr
    { ValueBinding{ _pVBPat = $1
                  , _pVBExpr = $3
                  , _pVBLoc = $1 ^. pPatLoc <~> $3 ^. pExpLoc }
    }
  | value_name expr_parameter_NE optional(type_constraint) '=' expr
    { let namePat = Pattern{ _pPatDesc = PPatVar $ getIdent $1, _pPatLoc = $1 <-> $1 } in
      ValueBinding{ _pVBPat = namePat
                  , _pVBExpr = mkFunExpr $2 $3 $5
                  , _pVBLoc = $1 ^. loc <~> $5 ^. pExpLoc }
    }

-- Type definitions
typedef :: { TypeDeclaration }
  : optional(type_params) typeconstr_name type_information
    { TypeDeclaration{ _pTypeName = getIdent $2
                     , _pTypeParams = maybe [] toList $1
                     , _pTypeKind = $3
                     , _pTypeLoc = (maybe ($2 ^. loc) ((view pTypLoc) . last) $1) <~> $2 ^. loc } -- FIXME: type_informatiom MUST have location
    }

type_information :: { TypeKind }
  : {- empty -} { PTypeAbstract Nothing }
  | '=' type_equation { PTypeAbstract (Just $2) }
  | '=' type_representation { PTypeVariant $2 }

type_equation :: { CoreType }
  : typexpr { $1 }

type_representationRev :: { [ConstructorDeclaration] }
  : constr_decl { [$1] }
  | '|' constr_decl { [$2] }
  | type_representationRev '|' constr_decl { $3 : $1 }

type_representation :: { [ConstructorDeclaration] }
  : '|' { [] }
  | type_representationRev { $1 }

-- For some LR related reason following code will give shift/reduce conflicts
-- -- type_representation :: { NonEmpty ConstructorDeclaration }
-- --   : lsepBy1Preceeded(constr_decl, '|') { $1 }
-- One above -- won't

type_params :: { NonEmpty CoreType }
  : type_param { pure $1 }
  | '(' sepBy1(type_param, ',') ')' { $2 }

type_param :: { CoreType }
  : '\'' ident { CoreType{ _pTyp = PTypVar $ getIdent $2, _pTypLoc = $1 <-> $2 } }

of_constr_args :: { NonEmpty CoreType }
  : 'of' constr_args { $2 }

constr_decl :: { ConstructorDeclaration }
  :
  constr_name optional(of_constr_args)
    { ConstructorDeclaration{ _pCDName = getIdent $1
                            , _pCDArgs = maybe [] toList $2
                            , _pCDLoc = $1 ^. loc <~> maybe ($1 ^. loc) ((view pTypLoc) . last) $2 }
    }
  |
  '[' ']' optional(of_constr_args)
    { ConstructorDeclaration{ _pCDName = "[]"
                            , _pCDArgs = maybe [] toList $3
                            , _pCDLoc = $1 ^. loc <~> maybe ($2 ^. loc) ((view pTypLoc) . last) $3 }
    }
  | '(' '::' ')' optional(of_constr_args)
    { ConstructorDeclaration{ _pCDName = "::"
                            , _pCDArgs = maybe [] toList $4
                            , _pCDLoc = $1 ^. loc <~> maybe ($3 ^. loc) ((view pTypLoc) . last) $4 }
    }

constr_args :: { NonEmpty CoreType }
  : sepBy1(atomic_type, '*') { $1 }

-- Declarations and modules
module_expression :: { ModuleExpr }
  : optional(module_definition) many(module_item)
  { ModuleExpr{ _pMEDefinition = $1, _pMEStructure = $2 } }

module_definition :: { ModuleDefinition }
  : 'module' module_path
  { ModuleDefinition{ _pMDName = getLongident $2, _pMDLoc = $1 <-> last $2 } }

module_item :: { StructureItem }
  : 'open' module_path
    { PStrOpen $ OpenDeclaration{ _pODName = getLongident $2, _pODLoc = $1 <-> last $2 } }
  | 'let' rec sepBy1(let_binding, 'and') { PStrValue $2 $3 }
  | 'type' sepBy1(typedef, 'and') { PStrType $2 }

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

rec :: { RecFlag }
rec
  : {- empty -} { NonRecursive }
  | 'rec' { Recursive }

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
parseError :: Token -> Alex a
parseError token = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ (toString $ token ^. readStr)

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

getIdent :: Token -> Text
getIdent tok = tok ^. tokenType . _TokIdent . _2

getLongident :: NonEmpty Token -> Longident
getLongident = fmap getIdent

infix 7 <->
{-| Version of ('<~>') specialied to 'Token' -}
(<->) :: Token -> Token -> Location
lTok <-> rTok = (lTok ^. loc <~> rTok ^. loc)

infix 7 <~>
{- | Union of two 'Location's with priority one below of '^.' -}
(<~>) :: Location -> Location -> Location
lLoc <~> rLoc = if lLocStart < rLocEnd then Loc lLocStart rLocEnd else error msg
 where
  lLocStart = lLoc ^. startPos
  rLocEnd = rLoc ^. endPos
  msg = "Internal parser error: left token must go after the right token"

mkListPat :: Token -> NonEmpty Pattern -> Token -> Pattern
mkListPat lBracket list rBracket = foldr helper init list
 where
  init :: Pattern
  init = Pattern{ _pPatDesc = PPatConstruct (pure "[]") Nothing, _pPatLoc = rBracket <-> rBracket }
  helper :: Pattern -> Pattern -> Pattern
  helper x acc = Pattern
    { _pPatDesc = PPatConstruct (pure "::") (Just pat)
    , _pPatLoc = x ^. pPatLoc <~> rBracket ^. loc
    }
   where
    pat = Pattern{ _pPatDesc = PPatTuple x (pure acc), _pPatLoc = x ^. pPatLoc <~> acc ^. pPatLoc }

mkListExpr :: Token -> NonEmpty Expression -> Token -> Expression
mkListExpr lBracket list rBracket = foldr helper init list
 where
  init :: Expression
  init = Expression{ _pExpDesc = PExpConstruct (pure "[]") Nothing, _pExpLoc = rBracket <-> rBracket }
  helper :: Expression -> Expression -> Expression
  helper x acc = Expression
    { _pExpDesc = PExpConstruct (pure "::") (Just expr)
    , _pExpLoc = x ^. pExpLoc <~> rBracket ^. loc
    }
   where
    expr = Expression{ _pExpDesc = PExpTuple x (pure acc), _pExpLoc = x ^. pExpLoc <~> acc ^. pExpLoc }

getInfixIdent :: Token -> NonEmpty Text
getInfixIdent (Token (TokInfixSymbol0 op) _ _) = pure op
getInfixIdent (Token (TokInfixSymbol1 op) _ _) = pure op
getInfixIdent (Token (TokInfixSymbol2 op) _ _) = pure op
getInfixIdent (Token (TokInfixSymbol3 op) _ _) = pure op
getInfixIdent (Token (TokInfixSymbol4 op) _ _) = pure op
getInfixIdent (Token TokLor _ _) = pure "lor"
getInfixIdent (Token TokLxor _ _) = pure "lxor"
getInfixIdent (Token TokMod _ _) = pure "mod"
getInfixIdent (Token TokLand _ _) = pure "land"
getInfixIdent (Token TokLsl _ _) = pure "lsl"
getInfixIdent (Token TokLsr _ _) = pure "lsr"
getInfixIdent (Token TokAsr _ _) = pure "asr"
getInfixIdent (Token TokBoolAnd _ _) = pure "&&"
getInfixIdent (Token TokStar _ _) = pure "*"
getInfixIdent (Token TokPlus _ _) = pure "+"
getInfixIdent (Token TokMinus _ _) = pure "-"
getInfixIdent (Token TokEq _ _) = pure "="
getInfixIdent (Token TokBar _ _) = pure "|"
getInfixIdent (Token TokDoubleBar _ _) = pure "||"

mkFunExpr :: NonEmpty Pattern -> Maybe CoreType -> Expression -> Expression
mkFunExpr pats mType rhsExpr = foldr helper init pats
 where
  init :: Expression
  init = case mType of
           Just typ -> Expression{ _pExpDesc = PExpConstraint rhsExpr typ
                                  , _pExpLoc = typ ^. pTypLoc <~> rhsExpr ^. pExpLoc
                                  }
           Nothing -> rhsExpr
  helper :: Pattern -> Expression -> Expression
  helper pat acc = Expression{ _pExpDesc = PExpFunction pat acc, _pExpLoc = pat ^. pPatLoc <~> acc ^. pExpLoc }

parseLamagraphML :: Text -> Either String ModuleExpr
parseLamagraphML text = runAlex text pLamagraphML
}
