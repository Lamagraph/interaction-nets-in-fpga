module Lamagraph.Compiler.Parser.LexerTest (lexerUnitTests) where

import Relude

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.LexerUtils

getTokenTypes :: Either String [Token] -> Either String [TokenType]
getTokenTypes tokens = tokens & _Right %~ toListOf (traverse . tokenType)

getTokenTypesFromText :: Text -> Either String [TokenType]
getTokenTypesFromText = getTokenTypes . scanner

skipWhitespace :: TestTree
skipWhitespace =
  testCase "Skip whitespace" $ do
    getTokenTypesFromText " \t\r\n" @?= Right [TokEOF]

oneLevelMultilineComment :: TestTree
oneLevelMultilineComment =
  testCase "Skip unnested multiline comment" $ do
    getTokenTypesFromText "(* This is one level comment *)" @?= Right [TokEOF]

nestedMultilineComments :: TestTree
nestedMultilineComments =
  testCase "Handle nested multiline comments" $ do
    getTokenTypesFromText comment @?= Right [TokEOF]
 where
  comment =
    "(* First level of comment \n\
    \ (* Second level of comment*) \n\
    \ *)"

singleLineComment :: TestTree
singleLineComment =
  testCase "Skip single-line comments" $ do
    getTokenTypesFromText "-- This is single line comment" @?= Right [TokEOF]

noMixSingleLineAndMultiline :: TestTree
noMixSingleLineAndMultiline =
  testCase "No mix of single-line and multiline comments" $ do
    getTokenTypesFromText comment @?= Left "lexical error at line 2, column 2: Closing comment before opening"
 where
  comment =
    "-- (* \n\
    \ *)"

capitalizedIdent :: TestTree
capitalizedIdent =
  testCase "Lex capitalized ident" $ do
    getTokenTypesFromText "MyModule_42'meow" @?= Right [TokIdent Capitalized "MyModule_42'meow", TokEOF]

lowercaseIdent :: TestTree
lowercaseIdent =
  testCase "Lex lowercase ident" $ do
    getTokenTypesFromText "_myType_42'meow" @?= Right [TokIdent Lowercase "_myType_42'meow", TokEOF]

integerLiteral :: TestTree
integerLiteral =
  testCase "Lex Int literal" $ do
    getTokenTypesFromText "42" @?= Right [TokInt 42, TokEOF]

int32Literal :: TestTree
int32Literal =
  testCase "Lex Int32 literal" $ do
    getTokenTypesFromText "42l" @?= Right [TokInt32 42, TokEOF]

uint32Literal :: TestTree
uint32Literal =
  testCase "Lex UInt32 literal" $ do
    getTokenTypesFromText "42ul" @?= Right [TokUInt32 42, TokEOF]

int64Literal :: TestTree
int64Literal =
  testCase "Lex Int64 literal" $ do
    getTokenTypesFromText "42L" @?= Right [TokInt64 42, TokEOF]

uint64Literal :: TestTree
uint64Literal =
  testCase "Lex UInt64 literal" $ do
    getTokenTypesFromText "42UL" @?= Right [TokUInt64 42, TokEOF]

allLetterKeywords :: TestTree -- Yep, strange name
allLetterKeywords =
  testCase "Lex all letter keywords" $ do
    getTokenTypesFromText str @?= Right tokens
 where
  str =
    "and asr else false fun if in land let \
    \lor lsl lsr lxor match mod module \
    \of open rec then true type when with"
  tokens =
    [ TokAnd
    , TokAsr
    , TokElse
    , TokFalse
    , TokFun
    , TokIf
    , TokIn
    , TokLand
    , TokLet
    , TokLor
    , TokLsl
    , TokLsr
    , TokLxor
    , TokMatch
    , TokMod
    , TokModule
    , TokOf
    , TokOpen
    , TokRec
    , TokThen
    , TokTrue
    , TokType
    , TokWhen
    , TokWith
    , TokEOF
    ]

allSymbolKeywords :: TestTree
allSymbolKeywords =
  testCase "Lex all symbol keywords" $ do
    getTokenTypesFromText str @?= Right tokens
 where
  str = "&& ' ( ) * + , - -> : :: ; = [ ] _ . | ||"
  tokens =
    [ TokBoolAnd
    , TokApostrophe
    , TokLeftPar
    , TokRightPar
    , TokStar
    , TokPlus
    , TokComma
    , TokMinus
    , TokArrow
    , TokColon
    , TokDoubleColon
    , TokSemicolon
    , TokEq
    , TokLeftBracket
    , TokRightBracket
    , TokWildcard
    , TokDot
    , TokBar
    , TokDoubleBar
    , TokEOF
    ]

infixSymbols :: TestTree
infixSymbols =
  testCase "Lex infix symbols" $ do
    getTokenTypesFromText "=@ <= >= |= &&& $! @. ^| += -** *+* / % **"
      @?= Right
        [ TokInfixSymbol0 "=@"
        , TokInfixSymbol0 "<="
        , TokInfixSymbol0 ">="
        , TokInfixSymbol0 "|="
        , TokInfixSymbol0 "&&&"
        , TokInfixSymbol0 "$!"
        , TokInfixSymbol1 "@."
        , TokInfixSymbol1 "^|"
        , TokInfixSymbol2 "+="
        , TokInfixSymbol2 "-**"
        , TokInfixSymbol3 "*+*"
        , TokInfixSymbol3 "/"
        , TokInfixSymbol3 "%"
        , TokInfixSymbol4 "**"
        , TokEOF
        ]

prefixSymbols :: TestTree
prefixSymbols =
  testCase "Lex prefix symbols" $ do
    getTokenTypesFromText "! != !@@. ?++ ~^|"
      @?= Right
        [ TokPrefixSymbol "!"
        , TokPrefixSymbol "!="
        , TokPrefixSymbol "!@@."
        , TokPrefixSymbol "?++"
        , TokPrefixSymbol "~^|"
        , TokEOF
        ]

regularString :: TestTree
regularString =
  testCase "Lex regular string" $ do
    getTokenTypesFromText "\"Regular string\""
      @?= Right [TokString "Regular string", TokEOF]

escapedCharsInString :: TestTree
escapedCharsInString =
  testCase "Lex escaped characters in string" $ do
    getTokenTypesFromText "\"\\n \\\" \\\' \\\\\""
      @?= Right [TokString "\n \" \' \\", TokEOF]

mixedCharsInString :: TestTree
mixedCharsInString =
  testCase "Lex mixed string" $ do
    getTokenTypesFromText "\"_myType_42\\\'meow is the best \\\\ identifier!\""
      @?= Right [TokString "_myType_42\'meow is the best \\ identifier!", TokEOF]

regularChar :: TestTree
regularChar =
  testCase "Lex regular char" $ do
    getTokenTypesFromText "\'~\'" @?= Right [TokChar '~', TokEOF]

escapedChars :: TestTree
escapedChars =
  testCase "Lex escaped chars" $ do
    getTokenTypesFromText "'\\n' '\\'' '\\\"' '\\\\'"
      @?= Right [TokChar '\n', TokChar '\'', TokChar '\"', TokChar '\\', TokEOF]

errorClosingCommentBeforeOpening :: TestTree
errorClosingCommentBeforeOpening =
  testCase "Error: closing comment before opening" $ do
    getTokenTypesFromText "*)" @?= Left "lexical error at line 1, column 1: Closing comment before opening"

errorUnfinishedEscapeChar :: TestTree
errorUnfinishedEscapeChar =
  testCase "Error: unfinished escape char" $ do
    getTokenTypesFromText "\"\\ a\"" @?= Left "lexical error at line 1, column 2: Unfinished escape character"

errorEOFInComment :: TestTree
errorEOFInComment =
  testCase "Error: EOF in comment" $ do
    getTokenTypesFromText "(* comment" @?= Left "lexical error: EOF while reading comment"

errorEOFInString :: TestTree
errorEOFInString =
  testCase "Error: EOF in comment" $ do
    getTokenTypesFromText "\"string" @?= Left "lexical error: EOF while reading string"

lexerUnitTests :: TestTree
lexerUnitTests =
  testGroup
    "Unit tests"
    [ skipWhitespace
    , oneLevelMultilineComment
    , nestedMultilineComments
    , singleLineComment
    , noMixSingleLineAndMultiline
    , capitalizedIdent
    , lowercaseIdent
    , integerLiteral
    , int32Literal
    , uint32Literal
    , int64Literal
    , uint64Literal
    , regularChar
    , escapedChars
    , regularString
    , escapedCharsInString
    , mixedCharsInString
    , infixSymbols
    , prefixSymbols
    , allLetterKeywords
    , allSymbolKeywords
    , errorClosingCommentBeforeOpening
    , errorUnfinishedEscapeChar
    , errorEOFInComment
    , errorEOFInString
    ]
