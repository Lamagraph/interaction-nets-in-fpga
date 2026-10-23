module Lamagraph.Compiler.Parser.LexerStateTest (lexerStateTests) where

import Relude hiding (show)

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit
import Prelude (show)

import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.LexerUtils
import Lamagraph.Compiler.Parser.SrcLoc

-- | Asserts that 'show'ing the value is non-empty (forcing the 'Show' method).
assertNonEmptyShow :: (Show a) => a -> Assertion
assertNonEmptyShow x =
  let s = show x
   in when (null s) (assertFailure "rendered empty")

sampleState :: AlexUserState
sampleState =
  AlexUserState
    { _lexerCommentDepth = 2
    , _lexerStringStartPos = Just (mkSrcLoc "f.lml" 1 2)
    , _lexerStringValue = "abc"
    }

lexerStateTests :: TestTree
lexerStateTests =
  testGroup
    "LexerTypes"
    [ testCase "alexInitUserState" $ do
        assertEqual "depth" (0 :: Int) (alexInitUserState ^. lexerCommentDepth)
        assertEqual "stringStartPos" (Nothing :: Maybe SrcLoc) (alexInitUserState ^. lexerStringStartPos)
        assertEqual "stringValue" "" (alexInitUserState ^. lexerStringValue)
    , testCase "AlexUserState fields + lenses" $ do
        assertEqual "depth" (2 :: Int) (sampleState ^. lexerCommentDepth)
        assertEqual "stringStartPos" (Just (mkSrcLoc "f.lml" 1 2) :: Maybe SrcLoc) (sampleState ^. lexerStringStartPos)
        assertEqual "stringValue" ("abc" :: Text) (sampleState ^. lexerStringValue)
    , testCase "AlexUserState Eq" $ do
        assertEqual "==" sampleState sampleState
        assertBool "/=" (sampleState /= alexInitUserState)
    , testCase "AlexUserState Show" $ assertNonEmptyShow sampleState
    , testCase "IdentType" $ do
        assertEqual "Capitalized ==" Capitalized Capitalized
        assertBool "Capitalized /= Lowercase" (Capitalized /= Lowercase)
        assertNonEmptyShow (Capitalized :: IdentType)
    , testCase "Token Eq + Show" $ do
        assertEqual "TokIdent ==" (TokIdent Lowercase "x") (TokIdent Lowercase "x")
        assertBool "TokIdent /=" (TokIdent Lowercase "x" /= TokIdent Capitalized "x")
        assertEqual "TokInt ==" (TokInt "42") (TokInt "42")
        assertEqual "TokChar ==" (TokChar 'a') (TokChar 'a')
        assertEqual "TokString ==" (TokString "hi") (TokString "hi")
        assertEqual "keyword ==" TokLet TokLet
        assertBool "keyword /=" (TokLet /= TokIf)
        assertEqual "symbol ==" TokDoubleColon TokDoubleColon
        assertBool "symbol /=" (TokColon /= TokDoubleColon)
        assertNonEmptyShow (TokIdent Lowercase "x" :: Token)
        assertNonEmptyShow TokEOF
    , testCase "lexer: block comment is skipped" $
        assertEqual
          "comment skipped"
          (Right [TokIdent Lowercase "hello", TokEOF])
          (getTokenTypesFromText "(* a comment *) hello")
    , testCase "lexer: nested comment is skipped" $
        assertEqual
          "nested comment skipped"
          (Right [TokIdent Lowercase "hello", TokEOF])
          (getTokenTypesFromText "(* outer (* inner *) outer *) hello")
    , testCase "lexer: string literal" $
        assertEqual "string" (Right [TokString "hi", TokEOF]) (getTokenTypesFromText "\"hi\"")
    , testCase "lexer: char literal" $ assertEqual "char" (Right [TokChar 'a', TokEOF]) (getTokenTypesFromText "'a'")
    , testCase "lexer: int literal" $ assertEqual "int" (Right [TokInt "42", TokEOF]) (getTokenTypesFromText "42")
    , testCase "lexer: capitalized ident" $
        assertEqual "capitalized" (Right [TokIdent Capitalized "Hello", TokEOF]) (getTokenTypesFromText "Hello")
    , testCase "lexer: double colon" $ assertEqual "::" (Right [TokDoubleColon, TokEOF]) (getTokenTypesFromText "::")
    ]
