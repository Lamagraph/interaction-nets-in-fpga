module Lamagraph.Compiler.TokenPassingCBVGolden (
  tokenPassingCBVLmlGolden,
  tokenPassingCBVParallelLmlGolden,
  tokenPassingCBVCoreGolden,
  tokenPassingCBVParallelCoreGolden,
) where

import Relude

import Data.Either.Extra (mapLeft)
import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Prettyprinter hiding (width)
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

lmlInputDir :: FilePath
lmlInputDir = baseGoldenTestsDir </> "nets" </> "token_passing_cbv_lml_in"

lmlOutputDir :: FilePath
lmlOutputDir = ".." </> "token_passing_cbv_lml_out"

coreOutputDir :: FilePath
coreOutputDir = baseGoldenTestsDir </> "nets" </> "token_passing_cbv_core_out"

renderUnbounded :: (Pretty a) => a -> LByteString
renderUnbounded x = encodeUtf8 $ renderLazy $ layoutPretty (LayoutOptions{layoutPageWidth = Unbounded}) (pretty x)

analyzeStats ::
  INsMachineStats ->
  -- | Reduction count, parallel width, parallel height
  ( Word64
  , Word64
  , Int
  )
analyzeStats INsMachineStats{reductionCounter, reductionWidthHistory} = (reductionCounter, maximum reductionWidthHistory, length reductionWidthHistory)

tokenPassingCBVLmlGolden :: IO TestTree
tokenPassingCBVLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] lmlInputDir
  return $
    testGroup
      "TokenPassingCBV Lml Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile lmlOutputDir) newExt
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = do
    fileLBS <- readFileLBS lmlFile
    let fileT = decodeUtf8 fileLBS
    parseTree <- fromEither $ mapLeft stringException $ parseLamagraphML fileT
    typedTree <- fromEither $ inferDef parseTree
    let binds = runMonadDesugar $ desugarLmlModule typedTree
    ((net, output), stats) <- runINsMachine $ do
      net <- coreBindsToTokenPassingCBV HashMap.empty [] [] binds
      output <- netToConfiguration net >>= reduce tokenPassingCBVRule >>= update >>= configurationToNet
      pure (net, output)
    let (counter, width, height) = analyzeStats stats
    pure $
      "Input net: "
        <> renderUnbounded net
        <> "\nOutput net: "
        <> renderUnbounded output
        <> "\nReduction count: "
        <> show counter
        <> "\nParallel width: "
        <> show width
        <> "\nParallel height: "
        <> show height

tokenPassingCBVParallelLmlGolden :: IO TestTree
tokenPassingCBVParallelLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] lmlInputDir
  return $
    testGroup
      "TokenPassingCBV Parallel Lml Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile lmlOutputDir) ("parallel." <> newExt)
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = do
    fileLBS <- readFileLBS lmlFile
    let fileT = decodeUtf8 fileLBS
    parseTree <- fromEither $ mapLeft stringException $ parseLamagraphML fileT
    typedTree <- fromEither $ inferDef parseTree
    let binds = runMonadDesugar $ desugarLmlModule typedTree
    ((net, output), stats) <- runINsMachine $ do
      net <- coreBindsToTokenPassingCBV HashMap.empty [] [] binds
      output <- netToConfiguration net >>= reduce tokenPassingCBVRuleParallel >>= update >>= configurationToNet
      pure (net, output)
    let (counter, width, height) = analyzeStats stats
    pure $
      "Input net: "
        <> renderUnbounded net
        <> "\nOutput net: "
        <> renderUnbounded output
        <> "\nReduction count: "
        <> show counter
        <> "\nParallel width: "
        <> show width
        <> "\nParallel height: "
        <> show height

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
    (resultNet, stats) <-
      runINsMachine $
        coreBindsToTokenPassingCBV HashMap.empty [] [] binds
          >>= netToConfiguration
          >>= reduce tokenPassingCBVRule
          >>= update
          >>= configurationToNet
    let (counter, width, height) = analyzeStats stats
    pure $
      "Input core:\n"
        <> renderUnbounded binds
        <> "\nResult net: "
        <> renderUnbounded resultNet
        <> "\nReduction count: "
        <> show counter
        <> "\nParallel width: "
        <> show width
        <> "\nParallel height: "
        <> show height

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
    (resultNet, stats) <-
      runINsMachine $
        coreBindsToTokenPassingCBV HashMap.empty [] [] binds
          >>= netToConfiguration
          >>= reduce tokenPassingCBVRuleParallel
          >>= update
          >>= configurationToNet
    let (counter, width, height) = analyzeStats stats
    pure $
      "Input core:\n"
        <> renderUnbounded binds
        <> "\nResult net: "
        <> renderUnbounded resultNet
        <> "\nReduction count: "
        <> show counter
        <> "\nParallel width: "
        <> show width
        <> "\nParallel height: "
        <> show height
