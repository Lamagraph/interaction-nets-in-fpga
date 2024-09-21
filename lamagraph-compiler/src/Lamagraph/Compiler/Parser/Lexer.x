-- Alex "Haskell code fragment top"
{
-- Some of the Alex generated code contains @undefinded@ which is considered
-- deprecated in Relude
{-# OPTIONS_GHC -Wno-deprecations #-}
-- Because of the active use of lenses, in this project field selectors are generally
-- disabled, but because Alex relies on them, they must be turned on explicitly
{-# LANGUAGE FieldSelectors #-}

module Lamagraph.Compiler.Parser.Lexer (
  Byte,
  AlexInput,
  AlexPosn(..),
  runAlex,
  Alex,
  alexError,
  alexGetInput,
  alexMonadScan
) where

import Relude
-- These functions must be used only for crashing the entire app,
-- because of the bug in Alex, not in this code
import Relude.Unsafe (fromJust, read)

import Control.Lens
import qualified Data.Text as Text

import Lamagraph.Compiler.Parser.LexerTypes
}
-- Alex "Wrapper"
%wrapper "monadUserState-strict-text"

-- Alex "Character set macros"

$digit = [0-9]
$letter = [a-zA-Z]
$capital_letter = [A-Z]
$lowercase_letter = [a-z]

$escape_sequence = [\\ \" \' \n]
$regular_char = [\ -\~] # $escape_sequence

$operator_char = [\! \$ \% & \* \+ \. \/ \: \< \= \> \? \@ \^ \| \~]

-- Alex "Regular expression macros"

-- Identifiers
@ident_tail = ( $letter | $digit | \_ | \' )*
@capitalized_ident = $capital_letter @ident_tail
@lowercase_ident = ( $lowercase_letter | \_ ) @ident_tail

-- Integer literals
@integer_literal = \-? $digit ( $digit | \_ )*
@int32_literal = @integer_literal l
@uint32_literal = @integer_literal ul
@int64_literal = @integer_literal L
@uint64_literal = @integer_literal UL

-- Operators
@infix_symbol = ( \= | \< | \> | \@ | \^ | \| | & | \+ | \- | \* | \/ | \$ | \% ) ( $operator_char )*
@prefix_symbol = (
  ( \! ( $operator_char )* )
  | ( ( \? | \~ ) ( $operator_char )+ )
)

-- Alex "Identifier"
lamagraphml :-

-- Alex "Rules"

<0> $white+ ;

<0> "(*" { enterNewComment `andBegin` state_comment }
<0> "*)" { alexErrorPos "Closing comment before opening" }
<state_comment> "(*" { embedComment }
<state_comment> "*)" { unembedComment }
<state_comment> . ;
<state_comment> \n ;

<0> "--" .* ;

<0> "and" { tok TokAnd }
<0> "asr" { tok TokAsr }
<0> "else" { tok TokElse }
<0> "false" { tok TokFalse }
<0> "fun" { tok TokFun }
<0> "if" { tok TokIf }
<0> "in" { tok TokIn }
<0> "land" { tok TokLand }
<0> "let" { tok TokLet }
<0> "lor" { tok TokLor }
<0> "lsl" { tok TokLsl }
<0> "lsr" { tok TokLsr }
<0> "lxor" { tok TokLxor }
<0> "match" { tok TokMatch }
<0> "mod" { tok TokMod }
<0> "module" { tok TokModule }
<0> "of" { tok TokOf }
<0> "open" { tok TokOpen }
<0> "rec" { tok TokRec }
<0> "then" { tok TokThen }
<0> "true" { tok TokTrue }
<0> "type" { tok TokType }
<0> "when" { tok TokWhen }
<0> "with" { tok TokWith }
<0> "&&" { tok TokBoolAnd }
<0> "'" { tok TokApostrophe }
<0> "(" { tok TokLeftPar }
<0> ")" { tok TokRightPar }
<0> "*" { tok TokStar }
<0> "+" { tok TokPlus }
<0> "," { tok TokComma }
<0> "-" { tok TokMinus }
<0> "->" { tok TokArrow }
<0> ":" { tok TokColon }
<0> "::" { tok TokDoubleColon }
<0> ";" { tok TokSemicolon }
<0> "=" { tok TokEq }
<0> "[" { tok TokLeftBracket }
<0> "]" { tok TokRightBracket }
<0> "_" { tok TokWildcard }
<0> "." { tok TokDot }
<0> "|" { tok TokBar }
<0> "||" { tok TokDoubleBar }

<0> @capitalized_ident { tokAnyIdent (TokIdent Capitalized) }
<0> @lowercase_ident { tokAnyIdent (TokIdent Lowercase) }

<0> @uint64_literal { tokAnyInt TokUInt64 }
<0> @uint32_literal { tokAnyInt TokUInt32 }
<0> @int64_literal { tokAnyInt TokInt64 }
<0> @int32_literal { tokAnyInt TokInt32 }
<0> @integer_literal { tokAnyInt TokInt }

<0> \' \\n \' { tokEscapedChar '\n' }
<0> \' \\\' \' { tokEscapedChar '\'' }
<0> \' \\\" \' { tokEscapedChar '\"' }
<0> \' \\\\ \' { tokEscapedChar '\\' }
<0> \' $regular_char \' { tokRegularChar }

<0> \" { enterNewString `andBegin` state_string }
<state_string> \\n { addCharToString '\n' }
<state_string> \\\" { addCharToString '\"' }
<state_string> \\\' { addCharToString '\'' }
<state_string> \\\\ { addCharToString '\\' }
<state_string> \\ { alexErrorPos "Unfinished escape character" }
<state_string> \" { leaveString `andBegin` 0 }
<state_string> $regular_char { addCurrentToString }

<0> \*\* ( $operator_char )* { tokAnyIdent TokInfixSymbol4 }
<0> (\* | \/ | \%) ( $operator_char )* { tokAnyIdent TokInfixSymbol3 }
<0> (\+ | \-) ( $operator_char )* { tokAnyIdent TokInfixSymbol2 }
<0> (\@ | \^) ( $operator_char )* { tokAnyIdent TokInfixSymbol1 }
<0> (\= | \< | \> | \| | & | \$) ( $operator_char )* { tokAnyIdent TokInfixSymbol0 }

<0> @prefix_symbol { tokAnyIdent TokPrefixSymbol }

-- Alex "Haskell code fragment bottom"
{
instance MonadState AlexUserState Alex where
  get :: Alex AlexUserState
  get = Alex $ \st -> Right (st, alex_ust st)
  put :: AlexUserState -> Alex ()
  put newState = Alex $ \st -> Right (st{alex_ust = newState}, ())

getEndPos :: AlexInput -> Int -> AlexPosn
getEndPos (startPosn, _, _, str) len = Text.foldl' alexMove startPosn $ Text.take len str

alexEOF :: Alex Token
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == state_comment) $ (alexError "lexical error: EOF while reading comment")
  when (startCode == state_string) $ (alexError "lexical error: EOF while reading string")
  (pos, _, _, _) <- alexGetInput
  return $ Token TokEOF (Loc pos pos) ""

enterNewComment :: AlexAction Token
enterNewComment input len = do
  lexerCommentDepth .= 1
  skip input len

embedComment :: AlexAction Token
embedComment input len = do
  lexerCommentDepth += 1
  skip input len

unembedComment :: AlexAction Token
unembedComment input len = do
  lexerCommentDepth -= 1
  commentDepth <- use lexerCommentDepth
  when (commentDepth == 0) $ alexSetStartCode 0
  skip input len

tokAnyIdent :: (Text -> TokenType) -> AlexAction Token
tokAnyIdent ctor input@(startPosn, _, _, str) len = do
  return Token
    { _tokenType = ctor $ Text.take len str
    , _loc = Loc startPosn $ getEndPos input len
    , _readStr = str
    }

tokAnyInt :: Read a => (a -> TokenType) -> AlexAction Token
tokAnyInt ctor input@(startPosn, _, _, str) len = do
  let num = read $ toString $ Text.dropWhileEnd isIntSuffix $ Text.take len str
  return Token
    { _tokenType = ctor num
    , _loc = Loc startPosn $ getEndPos input len
    , _readStr = str
    }
  where
    isIntSuffix :: Char -> Bool
    isIntSuffix 'u' = True
    isIntSuffix 'U' = True
    isIntSuffix 'l' = True
    isIntSuffix 'L' = True
    isIntSuffix _ = False

tok :: TokenType -> AlexAction Token
tok ctor input@(startPosn, _, _, str) len = do
  return Token
    { _tokenType = ctor
    , _loc = Loc startPosn $ getEndPos input len
    , _readStr = str
    }

enterNewString :: AlexAction Token
enterNewString (startPosn, _, _, str) len = do
  lexerStringStartPos .= Just startPosn
  lexerStringValue .= ""
  lexerReadString .= Text.take len str
  alexMonadScan

addCharToString :: Char -> AlexAction Token
addCharToString char (_, _, _, str) len = do
  lexerStringValue <>= Text.singleton char
  lexerReadString <>= Text.take len str
  alexMonadScan

leaveString :: AlexAction Token
leaveString input@(_, _, _, str) len = do
  lexerReadString <>= Text.take len str

  -- I wish I knew how to write this clearer
  tokenType' <- use lexerStringValue
  startPos' <- use lexerStringStartPos
  lexerReadString' <- use lexerReadString

  lexerStringStartPos .= Nothing
  lexerStringValue .= ""
  lexerReadString .= ""

  return
    Token
      { _tokenType = TokString tokenType'
      , _loc = Loc (fromJust startPos') $ getEndPos input len
      , _readStr = lexerReadString'
      }

addCurrentToString :: AlexAction Token
addCurrentToString input@(_, _, _, str) len = do
  let char = Text.head $ Text.take len str
  addCharToString char input len

tokRegularChar :: AlexAction Token
tokRegularChar input@(startPosn, _, _, str) len = do
  let fullStr = Text.take len str
  return Token
    { _tokenType = TokChar $ Text.head $ Text.dropAround (== '\'') fullStr
    , _loc = Loc startPosn $ getEndPos input len
    , _readStr = fullStr
    }

tokEscapedChar :: Char -> AlexAction Token
tokEscapedChar char input@(startPosn, _, _, str) len = do
  return Token
    { _tokenType = TokChar char
    , _loc = Loc startPosn $ getEndPos input len
    , _readStr = Text.take len str
    }

alexErrorPos :: String -> AlexAction a
alexErrorPos msg ((AlexPn _ line column), _, _, _) _ = Alex $ const $ Left fullMsg
  where
    fullMsg = "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ msg
}
