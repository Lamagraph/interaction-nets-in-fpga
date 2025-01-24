import Relude

import Test.Tasty
import Test.Tasty.Hedgehog

import Lamagraph.Compiler.Core.AnfCoreGolden
import Lamagraph.Compiler.Core.LambdaLiftingGolden
import Lamagraph.Compiler.Core.LlvmCoreGolden
import Lamagraph.Compiler.Core.PrettyCoreGolden
import Lamagraph.Compiler.Parser.LexerTest
import Lamagraph.Compiler.Parser.ParserRoundtrip
import Lamagraph.Compiler.Parser.PrettyAstGolden
import Lamagraph.Compiler.Parser.PrettyLmlGolden
import Lamagraph.Compiler.Typechecker.PrettyTypedGolden

main :: IO ()
main = do
  parserTests' <- parserTests
  typecheckerTests' <- typeCheckerTests
  coreTests' <- coreTests
  let tests = testGroup "Lamagraph Compiler" [lexerTests, parserTests', typecheckerTests', coreTests']
  defaultMain tests

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [lexerUnitTests]

parserTests :: IO TestTree
parserTests = do
  parserASTGolden <- parserPrettyAstGolden
  parserLmlGolden <- parserPrettyLmlGolden
  let roundtrip = testPropertyNamed "Parser roundtrip (AST -> LML -> AST)" "prop_ParserRoundtrip" prop_ParserRoundtrip
  return $ testGroup "Parser" [parserASTGolden, parserLmlGolden, roundtrip]

typeCheckerTests :: IO TestTree
typeCheckerTests = do
  tc <- typecheckerPrettyAstGolden
  pure $ testGroup "Typechecker" [tc]

coreTests :: IO TestTree
coreTests = do
  core <- corePrettyGolden
  coreLamLifting <- coreLamLiftGolden
  coreAnf <- coreAnfGolden
  llvmTests <- llvmGolden
  pure $ testGroup "Core" [core, coreLamLifting, coreAnf, llvmTests]
