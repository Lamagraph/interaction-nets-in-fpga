import Relude

import Test.Tasty

import Lamagraph.Compiler.Parser.LexerTest
import Lamagraph.Compiler.ParserGoldenTest

main :: IO ()
main = do
  parserTests' <- parserTests
  let tests = testGroup "" [lexerTests, parserTests']
  defaultMain tests

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [lexerUnitTests]

parserTests :: IO TestTree
parserTests = do
  parserGolden <- parserGoldenTests
  return $ testGroup "Parser" [parserGolden]
