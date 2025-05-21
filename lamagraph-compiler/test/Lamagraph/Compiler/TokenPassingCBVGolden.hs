module Lamagraph.Compiler.TokenPassingCBVGolden (tokenPassingCBVGolden) where

import Relude

import Data.Either.Extra (mapLeft)
import Data.HashMap.Strict qualified as HashMap
import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import UnliftIO.Exception

import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.GoldenCommon
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

renderUnbounded :: (Pretty a) => a -> LByteString
renderUnbounded x = encodeUtf8 $ renderLazy $ layoutPretty (LayoutOptions{layoutPageWidth = Unbounded}) (pretty x)

tokenPassingCBVGolden :: IO TestTree
tokenPassingCBVGolden = do
  lmlFiles <- findByExtension [lmlExt] inputDir
  return $
    testGroup
      "TokenPassingCBV Golden tests"
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
      (interface, activePairs) <- coreBindsToTokenPassingCBV HashMap.empty [] [] binds
      let net = Net{terms = interface, equations = activePairs}
      output <- netToConfiguration net >>= reduce tokenPassingCBVRule >>= update >>= configurationToNet
      pure $ "Input net: " <> renderUnbounded net <> "\nOutput net: " <> renderUnbounded output
    pure $ res <> "\nReduction count: " <> show counter
