{
{- I have no clue whether this make a difference in *any* speed,
but Happy docs recommend this flag -}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Lamagraph.Compiler.Parser (parseLamagraphML) where

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
  infix_symbol { Token{ _tokenType = TokInfixSymbol _ } }
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
  '{' { Token{ _tokenType = TokLeftCurly } }
  '}' { Token{ _tokenType = TokRightCurly } }
  '.' { Token{ _tokenType = TokDot } }
  '|' { Token{ _tokenType = TokBar } }
  '||' { Token{ _tokenType = TokDoubleBar } }
  '<' { Token{ _tokenType = TokLess } }
  '>' { Token{ _tokenType = TokGreater } }

-- Higher in the list, lower the precendce
%left '|'
%nonassoc below_COMMA
%left ',' -- typexpr (e, e, e)
%right '->' -- typexpr (t -> t ->t)
%right '::' -- e :: e :: e
%left '*' -- typeexpr ('t * 't)
%nonassoc constr_appl -- above '|' '::' ','
%nonassoc below_DOT
%nonassoc '.' -- idents
%nonassoc '(' ')' capitalized_ident lowercase_ident integer_literal
          int32_literal uint32_literal int64_literal uint64_literal
          char_literal string_literal

%%
-- Happy does pattern-matching againt %token directives, thus we need a rule for ident
ident :: { Token }
ident
  : lowercase_ident { $1 }
  | capitalized_ident { $1 }

-- Basic names
value_name :: { Token }
value_name
  : lowercase_ident { $1 }
  | '(' operator_name ')' { $2 }

operator_name :: { Token }
operator_name
  : prefix_symbol { $1 }
  | infix_op { $1 }

infix_op :: { Token }
infix_op
  : infix_symbol { $1 }
  | '*' { $1 }
  | '+' { $1 }
  | '-' { $1 }
  | '=' { $1 }
  | '<' { $1 }
  | '>' { $1 }
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

-- extra_constr_name :: { Token }
--   : '(' '::' ')' {}

typeconstr_name :: { Token }
typeconstr_name : lowercase_ident { $1 }

-- Qualified names
value_path :: { NonEmpty Token }
value_path : mkIdent(module_path, lowercase_ident) { $1 }

constr :: { NonEmpty Token }
  : mkIdent(module_path, capitalized_ident) { $1 }
  | 'true' { pure $1 }
  | 'false' { pure $1 }
  | '(' ')' { $1 :| [$2] }
  | '[' ']' { $1 :| [$2] }

typeconstr :: { NonEmpty Token }
typeconstr : mkIdent(module_path, lowercase_ident) { $1 }

module_path :: { NonEmpty Token }
module_path : mkIdent(module_path, capitalized_ident) { $1 }

-- Type expressions
typexpr :: { CoreType }
typexpr : function_type { $1 }

-- This was copied from OCaml
-- Otherwise there was plenty of reduce/reduce conflicts
function_type :: { CoreType }
  : tuple_type %prec '->' { $1 }
  | tuple_type '->' function_type
    { CoreType { _pTyp = PTypArrow $1 $3, _pTypLoc = $1 ^. pTypLoc <~> $3 ^. pTypLoc } }

tuple_type :: { CoreType }
tuple_type
  : atomic_type { $1 }
  | atomic_type lsepBy1(atomic_type, '*')
    { CoreType{ _pTyp = PTypTuple $1 $2, _pTypLoc = $1 ^. pTypLoc <~> (last $2) ^. pTypLoc } }

delimited_type :: { CoreType }
delimited_type : '(' function_type ')' { $2{ _pTypLoc = $1 <-> $3 } }

atomic_type :: { CoreType }
atomic_type
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
constant
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
  | value_name { Pattern{ _pPatDesc = PPatVar $ getIdent $1, _pPatLoc = $1 <-> $1 }}
  | '_' { Pattern{ _pPatDesc = PPatAny, _pPatLoc = $1 <-> $1 } }
  | constant { Pattern{ _pPatDesc = PPatConstant $1, _pPatLoc = $1 ^. pConstLoc <~> $1 ^. pConstLoc } }
  | constr { Pattern{ _pPatDesc = PPatConstruct (getLongident $1) Nothing, _pPatLoc = (head $1) <-> (last $1)} }
  | tuple_pattern '|' tuple_pattern { Pattern{ _pPatDesc = PPatOr $1 $3, _pPatLoc = $1 ^. pPatLoc <~> $3 ^. pPatLoc } }
  | constr tuple_pattern %prec constr_appl
    { Pattern{ _pPatDesc = PPatConstruct (getLongident $1) (Just $2)
             , _pPatLoc = (head $1) ^. loc <~> $2 ^. pPatLoc } }
  | '[' tuple_pattern lsepBy1(tuple_pattern, ';') '>' ']'
    { let lhs = Pattern{ _pPatDesc = PPatTuple $2 $3, _pPatLoc = $2 ^. pPatLoc <~> (last $3) ^. pPatLoc } in
      let rhs = Pattern{ _pPatDesc = PPatConstruct (pure "[]") Nothing, _pPatLoc = $5 <-> $5 } in
      Pattern{ _pPatDesc = PPatConstruct (pure "::")
                (Just $ Pattern{ _pPatDesc = PPatTuple lhs (pure rhs), _pPatLoc = $1 <-> $1 })
             , _pPatLoc = $1 <-> $5 } }
  | tuple_pattern '::' tuple_pattern
    { Pattern{ _pPatDesc = PPatConstruct (pure "::")
                (Just $ Pattern{ _pPatDesc = PPatTuple $1 (pure $3), _pPatLoc = $1 ^. pPatLoc <~> $3 ^. pPatLoc })
             , _pPatLoc = $2 <-> $2 } }

-- Expressions
expr :: { Expression }
expr : fun_expr { $1 }

-- -- tuple_expr :: { Expression }
-- -- tuple_expr
-- --   : atomic_expr { $1 }

-- fun_expr :: { Expression }
-- fun_expr
--   : simple_expr { $1 }
--   -- | simple_expr manyNE(simple_expr)
--   --   { Expression{ _pExpDesc = PExpApply $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }
--     -- | 'if' fun_expr 'then' fun_expr 'else' fun_expr
--     --   { Expression{ _pExpDesc = PExpIfThenElse $2 $4 $6, _pExpLoc = $1 ^. loc <~> $6 ^. pExpLoc } }
--   -- | expr lsepBy1(expr, ',') %prec below_COMMA
--   --   { Expression{ _pExpDesc = PExpTuple $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }

fun_expr :: { Expression }
  : simple_expr %prec below_DOT { $1 }
  -- | simple_expr manyNE(fun_expr)
  --   { Expression{ _pExpDesc = PExpApply $1 $2, _pExpLoc = $1 ^. pExpLoc <~> (last $2) ^. pExpLoc } }

simple_expr :: { Expression }
  : value_path
    { Expression{ _pExpDesc = PExpIdent $ getLongident $1, _pExpLoc = head $1 <-> last $1 } }
  -- | constant
  --   { Ex

-- simple_expr :: { Expression }
-- simple_expr
--   : '(' fun_expr ')' { $2{ _pExpLoc = $1 <-> $3 } }
--   | '(' fun_expr type_constraint ')' { Expression{ _pExpDesc = PExpConstraint $2 $3, _pExpLoc = $1 <-> $4 } }
--   -- | '(' fun_expr { error "Aa" }
--   | value_path
--     { Expression{ _pExpDesc = PExpIdent $ getLongident $1, _pExpLoc = head $1 <-> last $1 } }
--   -- | constant
--   --   { Expression{ _pExpDesc = PExpConstant $1, _pExpLoc = $1 ^. pConstLoc <~> $1 ^. pConstLoc } }

let_binding :: { ValueBinding }
let_binding
  : pattern '=' expr
    { ValueBinding{ _pVBPat = $1
                  , _pVBExpr = $3
                  , _pVBConstraint = Nothing
                  , _pVBLoc = $1 ^. pPatLoc <~> $3 ^. pExpLoc }
    }
  -- | value_name many()

-- type_constraint :: { CoreType }
-- type_constraint : ':' typexpr { $2 }

-- parameter

-- Declarations and modules
module_expression :: { ModuleExpr }
module_expression : optional(module_definition) many(module_item)
  { ModuleExpr{ _pMEDefinition = $1, _pMEStructure = $2 } }

module_definition :: { ModuleDefinition }
module_definition : 'module' module_path
  { ModuleDefinition{ _pMDName = getLongident $2, _pMDLoc = $1 <-> last $2 } }

module_item :: { StructureItem }
module_item
  : 'open' module_path
    { PStrOpen $ OpenDeclaration{ _pODName = getLongident $2, _pODLoc = $1 <-> last $2 } }
  | 'let' rec sepBy1(let_binding, 'and') { PStrValue $2 $3 }
  | expr { PStrEval $1 }

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

parseLamagraphML :: Text -> Either String ModuleExpr
parseLamagraphML text = runAlex text pLamagraphML
}
