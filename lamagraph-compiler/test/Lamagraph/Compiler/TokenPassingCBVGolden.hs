{-# LANGUAGE DuplicateRecordFields #-}

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
import Lamagraph.Compiler.Driver
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

data OutputStats = OutputStats
  {reductionCount :: Word64, reductionWidthHistory :: [Word64], maxParallelWidth :: Word64, parallelHeight :: Int}

analyzeStats :: INsMachineStats -> OutputStats
analyzeStats INsMachineStats{reductionCounter, reductionWidthHistory} =
  OutputStats
    { reductionCount = reductionCounter
    , reductionWidthHistory = reverse reductionWidthHistory
    , maxParallelWidth = maximum reductionWidthHistory
    , parallelHeight = length reductionWidthHistory
    }

mkOutput :: (Pretty a, Pretty label) => a -> Net label -> OutputStats -> LByteString
mkOutput input output OutputStats{reductionCount, reductionWidthHistory, maxParallelWidth, parallelHeight} =
  "Input: "
    <> renderUnbounded input
    <> "\nOutput: "
    <> renderUnbounded output
    <> "\nReduction count: "
    <> show reductionCount
    <> "\nParallel width: "
    <> show maxParallelWidth
    <> "\nParallel height: "
    <> show parallelHeight
    <> "\nParallel width history: "
    <> show reductionWidthHistory

lmlHelper :: Rule TokenPassingCBV -> FilePath -> IO LByteString
lmlHelper rule lmlFile = do
  fileLBS <- readFileLBS lmlFile
  let fileT = decodeUtf8 fileLBS
  parseTree <- fromEither $ mapLeft stringException $ parseLamagraphML fileT
  let resolvedTree = resolveModuleDefEnv parseTree
  typedTree <- fromEither $ inferDef resolvedTree
  let binds = runMonadDesugar $ desugarLmlModule typedTree
  ((net, output), stats) <- runINsMachine $ do
    net <- coreBindsToTokenPassingCBV HashMap.empty [] [] binds
    output <- netToConfiguration net >>= reduce rule >>= update >>= configurationToNet
    pure (net, output)
  pure $ mkOutput net output (analyzeStats stats)

tokenPassingCBVLmlGolden :: IO TestTree
tokenPassingCBVLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] lmlInputDir
  return $
    testGroup
      "TokenPassingCBV Lml Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (lmlHelper tokenPassingCBVRule lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile lmlOutputDir) newExt
      ]

tokenPassingCBVParallelLmlGolden :: IO TestTree
tokenPassingCBVParallelLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] lmlInputDir
  return $
    testGroup
      "TokenPassingCBV Parallel Lml Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (lmlHelper tokenPassingCBVRuleParallel lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile lmlOutputDir) ("parallel." <> newExt)
      ]

coreTests :: [(String, [CoreBind])]
coreTests =
  [ ("factRecZero", resFactRecZero)
  , ("factExplicitRecZero", resFactNonRecZero)
  , ("factRecOne", resFactRecOne)
  , ("factRecTwo", resFactRecTwo)
  ]

coreHelper :: Rule TokenPassingCBV -> [CoreBind] -> IO LByteString
coreHelper rule binds = do
  (resultNet, stats) <-
    runINsMachine $
      coreBindsToTokenPassingCBV HashMap.empty [] [] binds
        >>= netToConfiguration
        >>= reduce rule
        >>= update
        >>= configurationToNet
  pure $ mkOutput binds resultNet (analyzeStats stats)

tokenPassingCBVCoreGolden :: TestTree
tokenPassingCBVCoreGolden =
  testGroup
    "TokenPassingCBV Core Golden tests"
    [ goldenVsString name resultFilename (coreHelper tokenPassingCBVRule binds)
    | (name, binds) <- coreTests
    , let resultFilename = coreOutputDir </> (name ++ ".out")
    ]

tokenPassingCBVParallelCoreGolden :: TestTree
tokenPassingCBVParallelCoreGolden =
  testGroup
    "TokenPassingCBV Parallel Core Golden tests"
    [ goldenVsString name resultFilename (coreHelper tokenPassingCBVRuleParallel binds)
    | (name, binds) <- coreTests
    , let resultFilename = coreOutputDir </> (name ++ ".parallel.out")
    ]
