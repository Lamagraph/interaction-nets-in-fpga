{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tests.GraphRewriting.SKI where

import qualified Clash.Prelude as Clash
import qualified Common.Term as Term
import Core.Node
import Data.Maybe
import qualified GraphRewriting.Graph as GR
import GraphRewriting.SKI
import GraphRewriting.Translator as Translator
import INet.Graph
import INet.Net
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Tests.GraphRewriting.SKIExpectedNets
import Prelude as P

translateSKITerm :: Term.Expr -> Net 2 SKINet
translateSKITerm t = Translator.translate graph
 where
  graph = fromTerm t

translateSKIFile :: FilePath -> IO (Net 2 SKINet)
translateSKIFile file = do
  term <- Term.parseFile file
  pure $ translateSKITerm term

countOfActivePorts :: Core.Node.Node pn at -> Int
countOfActivePorts (Core.Node.Node _ sp _) = length $ filter isJust $ Clash.toList sp

skiExamplesPath :: FilePath
skiExamplesPath = "tests" </> "GR-examples" </> "ski"

bluebirdPath :: FilePath
bluebirdPath = skiExamplesPath </> "bluebird.ski"
cardinalPath :: FilePath
cardinalPath = skiExamplesPath </> "cardinal.ski"
baldEaglePath :: FilePath
baldEaglePath = skiExamplesPath </> "bald-eagle.ski"

goldenTranslateBluebird :: IO TestTree
goldenTranslateBluebird = do
  actualNet <- translateSKIFile bluebirdPath
  let expectedNet = bluebirdINet
  pure $ testCase "bluebird.ski" (actualNet @?= expectedNet)

unitTranslateCardinal :: IO TestTree
unitTranslateCardinal = do
  actualNet <- translateSKIFile cardinalPath
  let expectedNet = cardinalINet
  pure $ testCase "cardinal.ski" (actualNet @?= expectedNet)

baldEagleNodesCount :: IO TestTree
baldEagleNodesCount = do
  graph <- fromTerm <$> Term.parseFile baldEaglePath
  let expectedCountOfNodes = length $ GR.nodeMap graph
      actualCountOfNodes = length $ filter isJust $ Clash.toList (Translator.translate graph :: Net 2 SKINet)
  pure $ testCase "bald-eagle.ski" (actualCountOfNodes @?= expectedCountOfNodes)

baldEagleEdgesCount :: IO TestTree
baldEagleEdgesCount = do
  graph <- fromTerm <$> Term.parseFile baldEaglePath
  let expectedCountOfNodes = 2 * length (GR.edgeMap graph)
      actualCountOfNodes =
        sum $ map (maybe 0 countOfActivePorts) $ Clash.toList (Translator.translate graph :: Net 2 SKINet)
  pure $ testCase "bald-eagle.ski" (actualCountOfNodes @?= expectedCountOfNodes)

goldenTranslation :: IO TestTree
goldenTranslation = do
  bluebirdTranslate <- goldenTranslateBluebird
  cardinalTranslate <- unitTranslateCardinal
  pure $ testGroup "golden" [bluebirdTranslate, cardinalTranslate]

nodesCountTests :: IO TestTree
nodesCountTests = do
  baldEagle <- baldEagleNodesCount
  pure $ testGroup "nodes numbers are equal" [baldEagle]

edgesCountTests :: IO TestTree
edgesCountTests = do
  baldEagle <- baldEagleNodesCount
  pure $ testGroup "edges numbers are equal" [baldEagle]

skiUnitTests :: IO TestTree
skiUnitTests = do
  goldenTests <- goldenTranslation
  nodesCount <- nodesCountTests
  edgesCount <- edgesCountTests
  pure $ testGroup "ski translation" [goldenTests, nodesCount, edgesCount]
