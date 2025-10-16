import Relude

import Test.Tasty
import Test.Tasty.Hedgehog

import Lamagraph.Compiler.Core.PrettyCoreGolden
import Lamagraph.Compiler.Eval.EvalGolden
import Lamagraph.Compiler.Module.Typechecker.TypecheckerModuleGolden
import Lamagraph.Compiler.NetsGolden
import Lamagraph.Compiler.Parser.LexerTest
import Lamagraph.Compiler.Parser.ParserRoundtrip
import Lamagraph.Compiler.Parser.PrettyAstGolden
import Lamagraph.Compiler.Parser.PrettyLmlGolden
import Lamagraph.Compiler.TokenPassingCBVGolden
import Lamagraph.Compiler.Typechecker.PrettyTypedGolden

main :: IO ()
main = do
  parserTests' <- parserTests
  typecheckerTests' <- typeCheckerTests
  coreTests' <- coreTests
  evalTests' <- evalTests
  netsTests' <- netsTest
  moduleTypeCheckerTests' <- moduleTypeCheckerTests
  let tests =
        testGroup
          "Lamagraph Compiler"
          [lexerTests, parserTests', typecheckerTests', moduleTypeCheckerTests', coreTests', evalTests', netsTests']
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

moduleTypeCheckerTests :: IO TestTree
moduleTypeCheckerTests = do
  tc <- typecheckerPrettyModuleGolden
  pure $ testGroup "Module Typechecker" [tc]

coreTests :: IO TestTree
coreTests = do
  core <- corePrettyGolden
  pure $ testGroup "Core" [core]

evalTests :: IO TestTree
evalTests = do
  eval <- evalGolden
  pure $ testGroup "Eval" [eval]

netsTest :: IO TestTree
netsTest = do
  initialConfiguration <- initialConfigurationGolden
  reducedConfiguration <- reducedConfigurationGolden
  outNet <- outNetGolden
  tokenPassingCBV <- tokenPassingCBVLmlGolden
  tokenPassingCBVParallel <- tokenPassingCBVParallelLmlGolden
  pure $
    testGroup
      "Nets"
      [ initialConfiguration
      , reducedConfiguration
      , outNet
      , tokenPassingCBV
      , tokenPassingCBVParallel
      , tokenPassingCBVCoreGolden
      , tokenPassingCBVParallelCoreGolden
      ]
