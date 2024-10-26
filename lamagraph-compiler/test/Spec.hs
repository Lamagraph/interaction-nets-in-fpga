import Relude

import Test.Tasty
import Test.Tasty.Hedgehog

import Lamagraph.Compiler.Parser.LexerTest
import Lamagraph.Compiler.Parser.ParserRoundtrip
import Lamagraph.Compiler.Parser.PrettyAstGolden
import Lamagraph.Compiler.Parser.PrettyLmlGolden

main :: IO ()
main = do
  parserTests' <- parserTests
  let tests = testGroup "Lamagraph Compiler" [lexerTests, parserTests']
  defaultMain tests

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [lexerUnitTests]

parserTests :: IO TestTree
parserTests = do
  parserASTGolden <- parserPrettyAstGolden
  parserLmlGolden <- parserPrettyLmlGolden
  let roundtrip = testPropertyNamed "Parser roundtrip (AST -> LML -> AST)" "prop_ParserRoundtrip" prop_ParserRoundtrip
  return $ testGroup "Parser" [parserASTGolden, parserLmlGolden, roundtrip]
