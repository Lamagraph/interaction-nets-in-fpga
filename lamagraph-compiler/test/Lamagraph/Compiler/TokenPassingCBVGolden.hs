module Lamagraph.Compiler.TokenPassingCBVGolden (tokenPassingCBVLmlGolden, tokenPassingCBVCoreGolden, tokenPassingCBVParallelCoreGolden) where

import Relude

import Data.Either.Extra (mapLeft)
import Data.HashMap.Strict qualified as HashMap
import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import UnliftIO.Exception

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Nets.Encodings.LambdaCalculusCore
import Lamagraph.Compiler.Nets.Encodings.TokenPassingCBV
import Lamagraph.Compiler.Nets.Processing
import Lamagraph.Compiler.Nets.Reduction
import Lamagraph.Compiler.Nets.Types
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Typechecker.Infer

newExt :: String
newExt = "out"

inputDir :: FilePath
inputDir = baseGoldenTestsDir </> "nets" </> "token_passing_cbv_in"

outputDir :: FilePath
outputDir = ".." </> "token_passing_cbv_out"

coreOutputDir :: FilePath
coreOutputDir = baseGoldenTestsDir </> "nets" </> "token_passing_cbv_core_out"

renderUnbounded :: (Pretty a) => a -> LByteString
renderUnbounded x = encodeUtf8 $ renderLazy $ layoutPretty (LayoutOptions{layoutPageWidth = Unbounded}) (pretty x)

tokenPassingCBVLmlGolden :: IO TestTree
tokenPassingCBVLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] inputDir
  return $
    testGroup
      "TokenPassingCBV Lml Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile outputDir) newExt
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = do
    fileLBS <- readFileLBS lmlFile
    let fileT = decodeUtf8 fileLBS
    parseTree <- fromEither $ mapLeft stringException $ parseLamagraphML fileT
    typedTree <- fromEither $ inferDef parseTree
    let binds = runMonadDesugar $ desugarLmlModule typedTree
    (res, counter) <- runINsMachine $ do
      net <- coreBindsToTokenPassingCBV HashMap.empty [] [] binds
      output <- netToConfiguration net >>= reduce tokenPassingCBVRule >>= update >>= configurationToNet
      pure $ "Input net: " <> renderUnbounded net <> "\nOutput net: " <> renderUnbounded output
    pure $ res <> "\nReduction count: " <> show counter

coreTests :: [(String, [CoreBind])]
coreTests =
  [ ("factRecZero", resFactRecZero)
  , ("factExplicitRecZero", resFactNonRecZero)
  , ("factRecOne", resFactRecOne)
  , ("factRecTwo", resFactRecTwo)
  ]

tokenPassingCBVCoreGolden :: TestTree
tokenPassingCBVCoreGolden =
  testGroup
    "TokenPassingCBV Core Golden tests"
    [ goldenVsString name resultFilename (helper binds)
    | (name, binds) <- coreTests
    , let resultFilename = coreOutputDir </> (name ++ ".out")
    ]
 where
  helper binds = do
    (resultNet, reductionCount) <-
      runINsMachine $
        coreBindsToTokenPassingCBV HashMap.empty [] [] binds
          >>= netToConfiguration
          >>= reduce tokenPassingCBVRule
          >>= update
          >>= configurationToNet
    pure $
      "Input core:\n"
        <> renderUnbounded binds
        <> "\nResult net: "
        <> renderUnbounded resultNet
        <> "\nReduction count: "
        <> show reductionCount

tokenPassingCBVParallelCoreGolden :: TestTree
tokenPassingCBVParallelCoreGolden =
  testGroup
    "TokenPassingCBV Parallel Core Golden tests"
    [ goldenVsString name resultFilename (helper binds)
    | (name, binds) <- coreTests
    , let resultFilename = coreOutputDir </> (name ++ ".parallel.out")
    ]
 where
  helper binds = do
    (resultNet, reductionCount) <-
      runINsMachine $
        coreBindsToTokenPassingCBV HashMap.empty [] [] binds
          >>= netToConfiguration
          >>= reduce tokenPassingCBVRuleParallel
          >>= update
          >>= configurationToNet
    pure $
      "Input core:\n"
        <> renderUnbounded binds
        <> "\nResult net: "
        <> renderUnbounded resultNet
        <> "\nReduction count: "
        <> show reductionCount
