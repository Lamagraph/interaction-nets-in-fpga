--------------------------------------
-- Alex "Haskell code fragment top" --
--------------------------------------
{
{- Some of the Alex generated code contains @undefinded@ which is considered
deprecated in Relude
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{- Because of the active use of lenses, in this project field selectors are generally
disabled, but because Alex relies on them, they must be turned on explicitly
-}
{-# LANGUAGE FieldSelectors #-}

{- | Module with Alex parser

Some of the exports aren't used in other modules, but are useful in docs.
-}
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
{- These functions must be used only for crashing the entire app,
because of the bug in Alex, not in this code
-}
import Relude.Unsafe (fromJust, read)

import Control.Lens
import qualified Data.Text as Text

import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.SrcLoc
}
--------------------
-- Alex "Wrapper" --
--------------------
%wrapper "monadUserState-strict-text"

---------------------------------
-- Alex "Character set macros" --
---------------------------------

$digit = [0-9]
$letter = [a-zA-Z]
$capital_letter = [A-Z]
$lowercase_letter = [a-z]

$escape_sequence = [\\ \" \' \n]
$regular_char = [\ -\~] # $escape_sequence -- # is a set difference

$operator_char = [\! \$ \% & \* \+ \. \/ \: \< \= \> \? \@ \^ \| \~]

--------------------------------------
-- Alex "Regular expression macros" --
--------------------------------------

-- Identifiers
@ident_tail = ( $letter | $digit | \_ | \' )*
@capitalized_ident = $capital_letter @ident_tail
@lowercase_ident = (
  ( $lowercase_letter @ident_tail)
  -- In constrast to the grammar @_@ is reserved as a wildcard and thus cannot be identifier
  | ( \_ ( $letter | $digit ) @ident_tail )
)

-- Integer literals
@integer_literal = \-? $digit ( $digit | \_ )*

-- Operators
@infix_symbol = ( \= | \< | \> | \@ | \^ | \| | & | \+ | \- | \* | \/ | \$ | \% ) ( $operator_char )*
@prefix_symbol = (
  ( \! ( $operator_char )* )
  | ( ( \? | \~ ) ( $operator_char )+ )
)

-----------------------
-- Alex "Identifier" --
-----------------------

lamagraphml :-

------------------
-- Alex "Rules" --
------------------

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

<0> @integer_literal { tokInt }

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

-----------------------------------------
-- Alex "Haskell code fragment bottom" --
-----------------------------------------
{
instance MonadState AlexUserState Alex where
  get :: Alex AlexUserState
  get = Alex $ \st -> Right (st, alex_ust st)
  put :: AlexUserState -> Alex ()
  put newState = Alex $ \st -> Right (st{alex_ust = newState}, ())

-- FIXME: Handle filenames correctly
alexPosnToSrcLoc :: AlexPosn -> SrcLoc
alexPosnToSrcLoc (AlexPn _ line column) = mkSrcLoc "" line column

alexPosnToSrcSpan :: AlexPosn -> AlexPosn -> SrcSpan
alexPosnToSrcSpan pos1 pos2 = mkSrcSpan (alexPosnToSrcLoc pos1) (alexPosnToSrcLoc pos2)

getEndPos :: AlexInput -> Int -> AlexPosn
getEndPos (startPosn, _, _, str) len = Text.foldl' alexMove startPosn $ Text.take len str

alexEOF :: Alex LToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == state_comment) $ (alexError "lexical error: EOF while reading comment")
  when (startCode == state_string) $ (alexError "lexical error: EOF while reading string")
  (pos, _, _, _) <- alexGetInput
  return $ L (alexPosnToSrcSpan pos pos) TokEOF

enterNewComment :: AlexAction LToken
enterNewComment input len = do
  lexerCommentDepth .= 1
  skip input len

embedComment :: AlexAction LToken
embedComment input len = do
  lexerCommentDepth += 1
  skip input len

unembedComment :: AlexAction LToken
unembedComment input len = do
  lexerCommentDepth -= 1
  commentDepth <- use lexerCommentDepth
  when (commentDepth == 0) $ alexSetStartCode 0
  skip input len

tokAnyIdent :: (Text -> Token) -> AlexAction LToken
tokAnyIdent ctor input@(startPosn, _, _, str) len = do
  return $ L
    (alexPosnToSrcSpan startPosn $ getEndPos input len)
    (ctor $ Text.take len str)

tokInt :: AlexAction LToken
tokInt input@(startPosn, _, _, str) len = do
  let num = read $ toString $ Text.take len str
  return $ L
    (alexPosnToSrcSpan startPosn $ getEndPos input len)
    (TokInt num)

tok :: Token -> AlexAction LToken
tok ctor input@(startPosn, _, _, _) len = do
  return $ L (alexPosnToSrcSpan startPosn $ getEndPos input len) ctor

enterNewString :: AlexAction LToken
enterNewString (startPosn, _, _, _) _ = do
  lexerStringStartPos .= Just (alexPosnToSrcLoc startPosn)
  lexerStringValue .= ""
  alexMonadScan

addCharToString :: Char -> AlexAction LToken
addCharToString char (_, _, _, _) _ = do
  lexerStringValue <>= Text.singleton char
  alexMonadScan

leaveString :: AlexAction LToken
leaveString input@(_, _, _, _) len = do
  tokenType' <- use lexerStringValue
  startPos' <- use lexerStringStartPos

  lexerStringStartPos .= Nothing
  lexerStringValue .= ""

  return $ L
    (mkSrcSpan (fromJust startPos') (alexPosnToSrcLoc $ getEndPos input len))
    (TokString tokenType')

addCurrentToString :: AlexAction LToken
addCurrentToString input@(_, _, _, str) len = do
  let char = Text.head $ Text.take len str
  addCharToString char input len

tokRegularChar :: AlexAction LToken
tokRegularChar input@(startPosn, _, _, str) len = do
  let fullStr = Text.take len str
  return $ L
    (alexPosnToSrcSpan startPosn $ getEndPos input len)
    (TokChar $ Text.head $ Text.dropAround (== '\'') fullStr)

tokEscapedChar :: Char -> AlexAction LToken
tokEscapedChar char input@(startPosn, _, _, _) len = do
  return $ L
    (alexPosnToSrcSpan startPosn $ getEndPos input len)
    (TokChar char)

-- TODO: Investigate 'String' there
alexErrorPos :: String -> AlexAction a
alexErrorPos msg ((AlexPn _ line column), _, _, _) _ = Alex $ const $ Left fullMsg
  where
    fullMsg = "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": " ++ msg
}
