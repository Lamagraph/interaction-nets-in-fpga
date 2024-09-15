module Lamagraph.Compiler.Parser.LexerTest (lexerTests) where

import Control.Lens
import Lamagraph.Compiler.Parser.Lexer
import Lamagraph.Compiler.Parser.LexerUtils
import Relude
import Test.Hspec

getTokenTypes :: Either String [Token] -> Either String [TokenType]
getTokenTypes tokens = tokens & _Right %~ toListOf (traverse . tokenType)

getTokenTypesFromText :: Text -> Either String [TokenType]
getTokenTypesFromText = getTokenTypes . scanner

skipWhitespace :: SpecWith ()
skipWhitespace =
  it "Skip whitespace" $ do
    getTokenTypesFromText " \t\r\n" `shouldBe` Right [TokEOF]

oneLevelMultilineComment :: SpecWith ()
oneLevelMultilineComment =
  it "Skip unnested multiline comment" $ do
    getTokenTypesFromText "(* This is one level comment *)" `shouldBe` Right [TokEOF]

nestedMultilineComments :: SpecWith ()
nestedMultilineComments =
  it "Handle nested multiline comments" $ do
    getTokenTypesFromText comment `shouldBe` Right [TokEOF]
 where
  comment =
    "(* First level of comment \n\
    \ (* Second level of comment*) \n\
    \ *)"

singleLineComment :: SpecWith ()
singleLineComment =
  it "Skip single-line comments" $ do
    getTokenTypesFromText "-- This is single line comment" `shouldBe` Right [TokEOF]

noMixSingleLineAndMultiline :: SpecWith ()
noMixSingleLineAndMultiline =
  it "No mix of single-line and multiline comments" $ do
    getTokenTypesFromText comment `shouldBe` Left "lexical error at line 2, column 2: Closing comment before opening"
 where
  comment =
    "-- (* \n\
    \ *)"

capitalizedIdent :: SpecWith ()
capitalizedIdent =
  it "Lex capitalized ident" $ do
    getTokenTypesFromText "MyModule_42'meow" `shouldBe` Right [TokIdent Capitalized "MyModule_42'meow", TokEOF]

lowercaseIdent :: SpecWith ()
lowercaseIdent =
  it "Lex lowercase ident" $ do
    getTokenTypesFromText "_myType_42'meow" `shouldBe` Right [TokIdent Lowercase "_myType_42'meow", TokEOF]

integerLiteral :: SpecWith ()
integerLiteral =
  it "Lex Int literal" $ do
    getTokenTypesFromText "42" `shouldBe` Right [TokInt 42, TokEOF]

int32Literal :: SpecWith ()
int32Literal =
  it "Lex Int32 literal" $ do
    getTokenTypesFromText "42l" `shouldBe` Right [TokInt32 42, TokEOF]

uint32Literal :: SpecWith ()
uint32Literal =
  it "Lex UInt32 literal" $ do
    getTokenTypesFromText "42ul" `shouldBe` Right [TokUInt32 42, TokEOF]

int64Literal :: SpecWith ()
int64Literal =
  it "Lex Int64 literal" $ do
    getTokenTypesFromText "42L" `shouldBe` Right [TokInt64 42, TokEOF]

uint64Literal :: SpecWith ()
uint64Literal =
  it "Lex UInt64 literal" $ do
    getTokenTypesFromText "42UL" `shouldBe` Right [TokUInt64 42, TokEOF]

allLetterKeywords :: SpecWith () -- Yep, strange name
allLetterKeywords =
  it "Lex all letter keywords" $ do
    getTokenTypesFromText str `shouldBe` Right tokens
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

allSymbolKeywords :: SpecWith ()
allSymbolKeywords =
  it "Lex all symbol keywords" $ do
    getTokenTypesFromText str `shouldBe` Right tokens
 where
  str = "&& ' ( ) * + , - -> : :: ; = [ ] _ { | } ."
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
    , TokLeftCurly
    , TokBar
    , TokRightCurly
    , TokDot
    , TokEOF
    ]

infixSymbols :: SpecWith ()
infixSymbols =
  it "Lex infix symbols" $ do
    getTokenTypesFromText "=@ |= >= <= <> &&& %"
      `shouldBe` Right
        [ TokInfixSymbol "=@"
        , TokInfixSymbol "|="
        , TokInfixSymbol ">="
        , TokInfixSymbol "<="
        , TokInfixSymbol "<>"
        , TokInfixSymbol "&&&"
        , TokInfixSymbol "%"
        , TokEOF
        ]

prefixSymbols :: SpecWith ()
prefixSymbols =
  it "Lex prefix symbols" $ do
    getTokenTypesFromText "! != !@@. ?++ ~^|"
      `shouldBe` Right
        [ TokPrefixSymbol "!"
        , TokPrefixSymbol "!="
        , TokPrefixSymbol "!@@."
        , TokPrefixSymbol "?++"
        , TokPrefixSymbol "~^|"
        , TokEOF
        ]

regularString :: SpecWith ()
regularString =
  it "Lex regular string" $ do
    getTokenTypesFromText "\"Regular string\""
      `shouldBe` Right [TokString "Regular string", TokEOF]

escapedCharsInString :: SpecWith ()
escapedCharsInString =
  it "Lex escaped characters in string" $ do
    getTokenTypesFromText "\"\\n \\\" \\\' \\\\\""
      `shouldBe` Right [TokString "\n \" \' \\", TokEOF]

mixedCharsInString :: SpecWith ()
mixedCharsInString =
  it "Lex mixed string" $ do
    getTokenTypesFromText "\"_myType_42\\\'meow is the best \\\\ identifier!\""
      `shouldBe` Right [TokString "_myType_42\'meow is the best \\ identifier!", TokEOF]

regularChar :: SpecWith ()
regularChar =
  it "Lex regular char" $ do
    getTokenTypesFromText "\'~\'" `shouldBe` Right [TokChar '~', TokEOF]

escapedChars :: SpecWith ()
escapedChars =
  it "Lex escaped chars" $ do
    getTokenTypesFromText "'\\n' '\\'' '\\\"' '\\\\'"
      `shouldBe` Right [TokChar '\n', TokChar '\'', TokChar '\"', TokChar '\\', TokEOF]

errorClosingCommentBeforeOpening :: SpecWith ()
errorClosingCommentBeforeOpening =
  it "Error: closing comment before opening" $ do
    getTokenTypesFromText "*)" `shouldBe` Left "lexical error at line 1, column 1: Closing comment before opening"

errorUnfinishedEscapeChar :: SpecWith ()
errorUnfinishedEscapeChar =
  it "Error: unfinished escape char" $ do
    getTokenTypesFromText "\"\\ a\"" `shouldBe` Left "lexical error at line 1, column 2: Unfinished escape character"

errorEOFInComment :: SpecWith ()
errorEOFInComment =
  it "Error: EOF in comment" $ do
    getTokenTypesFromText "(* comment" `shouldBe` Left "lexical error: EOF while reading comment"

errorEOFInString :: SpecWith ()
errorEOFInString =
  it "Error: EOF in comment" $ do
    getTokenTypesFromText "\"string" `shouldBe` Left "lexical error: EOF while reading string"

lexerTests :: SpecWith ()
lexerTests = do
  describe "Unit tests" $ do
    skipWhitespace
    oneLevelMultilineComment
    nestedMultilineComments
    singleLineComment
    noMixSingleLineAndMultiline
    capitalizedIdent
    lowercaseIdent
    integerLiteral
    int32Literal
    uint32Literal
    int64Literal
    uint64Literal
    regularChar
    escapedChars
    regularString
    escapedCharsInString
    mixedCharsInString
    infixSymbols
    prefixSymbols
    allLetterKeywords
    allSymbolKeywords
    errorClosingCommentBeforeOpening
    errorUnfinishedEscapeChar
    errorEOFInComment
    errorEOFInString
