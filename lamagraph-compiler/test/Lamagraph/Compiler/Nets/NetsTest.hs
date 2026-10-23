module Lamagraph.Compiler.Nets.NetsTest (netsUnitTests, netsExamplesGolden) where

import Relude hiding (show)

import Control.Exception (try)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Map.Strict qualified as Map
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Prelude (show)

import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Nets.Examples
import Lamagraph.Compiler.Nets.Processing
import Lamagraph.Compiler.Nets.Reduction
import Lamagraph.Compiler.Nets.Types
import Lamagraph.Compiler.Nets.Utils

-- * Examples golden tests

netsGoldenDir :: FilePath
netsGoldenDir = baseGoldenTestsDir </> "nets" </> "nets_unit"

{- | Golden 'show' of the example configurations and nets, and of one
reduction step applied to them (covering the peano and linear-lambda
examples end to end).
-}
netsExamplesGolden :: IO TestTree
netsExamplesGolden = do
  peano1 <- goldenConf "peanoConf1" peanoConf1
  peano2 <- goldenConf "peanoConf2" peanoConf2
  lLambda1 <- goldenConf "lLambdaConf1" lLambdaConf1
  peanoReduced <- goldenReduced "peanoReduce.peanoConf1" peanoConf1 peanoReduce
  lLambdaReduced <- goldenReduced "lLambdaReduce.lLambdaConf1" lLambdaConf1 lLambdaReduce
  let lLambdaNet = mkGolden "lLambdaNet1" (pack (show lLambdaNet1))
  pure $
    testGroup
      "Nets Examples Golden"
      [peano1, peano2, lLambda1, lLambdaNet, peanoReduced, lLambdaReduced]
 where
  mkGolden testName content = goldenVsString testName (netsGoldenDir </> (testName <.> "golden")) (pure content)
  goldenConf testName conf = do
    c <- runINsMachine conf
    pure (mkGolden testName (pack (show (fst c))))
  goldenReduced testName conf step = do
    c <- runINsMachine conf
    r <- runINsMachine (step (fst c))
    pure (mkGolden testName (pack (show (fst r))))

-- * Types unit tests

{- | A label that derives both 'Show' and 'Eq' (the example labels 'Peano'
and 'LLambda' only derive 'Show'), used to exercise the derived 'Eq'
instances on 'Term'/'AnnTerm'/'ThreadState'.
-}
data EqLabel = A1 | A2 deriving (Show, Eq)

{- | Runs the machine action, expecting it to raise an 'INsException' matched
by 'isError'; also forces the exception's 'Show'.
-}
expectException ::
  (Show label, Typeable label) =>
  String ->
  INsMachine (Configuration label) ->
  (INsException label -> Bool) ->
  TestTree
expectException testName action isError = testCase testName $ do
  r <- try (runINsMachine action)
  case r of
    Left err ->
      let shown = show err
       in if isError err
            then return ()
            else assertFailure shown
    Right _ -> assertFailure "expected an INsException"

cannotReduceConf :: Configuration Peano
cannotReduceConf =
  Configuration
    { heap = Map.empty
    , stack = Nothing :| []
    , phi = Map.empty
    , iface = []
    , cycles = Map.empty
    , threadState = Process (mkAnnVar 0, mkAnnVar 1)
    }

degenerateConf :: Configuration Peano
degenerateConf =
  Configuration
    { heap = Map.empty
    , stack = Just (mkAnnVar 0, mkAnnVar 1) :| []
    , phi = Map.empty
    , iface = []
    , cycles = Map.empty
    , threadState = Delist
    }

incorrectNet :: Net Peano
incorrectNet = Net{terms = [Var 1], equations = []}

exceptionTests :: TestTree
exceptionTests =
  testGroup
    "INsException ctors"
    [ expectException
        "CannotApplyAnyRuleException"
        (reduceStep peanoRule cannotReduceConf)
        ( \case
            CannotApplyAnyRuleException _ -> True
            _ -> False
        )
    , expectException
        "DelistFromEmptyStackException"
        ( reduceStep
            peanoRule
            (mkConfigurationWithDefault ([] :: [(AnnTerm Peano, AnnTerm Peano)]) Map.empty ([] :: [AnnTerm Peano]))
        )
        ( \case
            DelistFromEmptyStackException _ -> True
            _ -> False
        )
    , expectException
        "DegenerateConfigurationException"
        (reduceStep peanoRule degenerateConf)
        ( \case
            DegenerateConfigurationException _ -> True
            _ -> False
        )
    , expectException
        "IncorrectNet"
        (netToConfiguration incorrectNet)
        ( \case
            IncorrectNet _ _ -> True
            _ -> False
        )
    ]

sampleConfiguration :: Configuration Peano
sampleConfiguration =
  Configuration
    { heap = Map.fromList [(0, mkAnnVar 5)]
    , stack = Just (mkAnnVar 1, mkAnnVar 2) :| [Nothing]
    , phi = Map.fromList [(1, 2), (2, 1)]
    , iface = [mkAnnVar 3]
    , cycles = Map.fromList [(4, mkAnnVar 6)]
    , threadState = Process (mkAnnVar 7, AnnTerm [Nothing] (Agent Succ [Agent Zero []]))
    }

sampleNet :: Net Peano
sampleNet = Net{terms = [Var 1, Agent Succ [Var 2]], equations = [(Agent Zero [], Agent Add [Var 1, Var 3])]}

{- | Asserts that 'show'ing the value yields a non-empty string (forcing the
rendering to enter the derived 'Show' method).
-}
assertShowNonEmpty :: (Show a) => String -> a -> Assertion
assertShowNonEmpty label x =
  let s = show x
   in when (null s) (assertFailure (label <> " rendered empty"))

recordAndInstanceTests :: TestTree
recordAndInstanceTests =
  testGroup
    "Configuration / Net / stats fields + Eq instances"
    [ testCase "Configuration all fields (Show)" $ assertShowNonEmpty "Configuration" sampleConfiguration
    , testCase "Net fields (Show)" $ assertShowNonEmpty "Net" sampleNet
    , testCase "INsMachineStats fields (Show)" $ do
        (_, stats) <- runINsMachine (pure ())
        assertShowNonEmpty "INsMachineStats" stats
    , testCase "Term Eq" $ do
        assertEqual "Var ==" (Var 1 :: Term EqLabel) (Var 1)
        assertBool "Var /=" ((Var 1 :: Term EqLabel) /= Var 2)
        assertEqual "Agent ==" (Agent A1 [Var 1] :: Term EqLabel) (Agent A1 [Var 1])
        assertBool "Agent /=" ((Agent A1 [Var 1] :: Term EqLabel) /= Agent A1 [Var 2])
    , testCase "AnnTerm Eq" $ do
        assertEqual "AnnTerm ==" (AnnTerm [] (Var 1) :: AnnTerm EqLabel) (AnnTerm [] (Var 1))
        assertBool "AnnTerm /=" ((AnnTerm [Just 1] (Var 1) :: AnnTerm EqLabel) /= AnnTerm [Nothing] (Var 1))
    , testCase "ThreadState Eq" $ do
        let p = Process (mkAnnVar 0, mkAnnVar 1) :: ThreadState EqLabel
            e = Enlist [(mkAnnVar 0, mkAnnVar 1)] :: ThreadState EqLabel
            d = Delist :: ThreadState EqLabel
            c = Cycle (0, mkAnnVar 1) :: ThreadState EqLabel
        assertEqual "Process ==" p p
        assertBool "Process /= Enlist" (p /= e)
        assertEqual "Enlist ==" e e
        assertEqual "Delist ==" d d
        assertBool "Delist /= Process" (d /= p)
        assertEqual "Cycle ==" c c
    ]

netsUnitTests :: TestTree
netsUnitTests =
  testGroup
    "Nets Unit tests"
    [exceptionTests, recordAndInstanceTests]
