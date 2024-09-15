import Relude

import Lamagraph.Compiler.Parser.LexerTest
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  tests <- unitTests
  defaultMain tests

unitTests :: IO TestTree
unitTests = testSpec "Lexer" lexerTests
