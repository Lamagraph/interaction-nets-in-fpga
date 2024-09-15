import Relude

import Test.Tasty

import Lamagraph.Compiler.Parser.LexerTest

main :: IO ()
main = defaultMain lexerTests

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [lexerUnitTests]
