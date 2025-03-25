-- | Module with example of usage of Interaction Nets encoding and abstract machine
module Lamagraph.Compiler.Nets.Examples (
  Peano (..),
  peanoRule,
  peanoReduce,
  peanoConf1,
  peanoConf2,
  peanoNet1,
  peanoNet2,
  LLambda (..),
  lLambdaRule,
  lLambdaReduce,
  lLambdaConf1,
  lLambdaNet1,
) where

import Relude

import Data.Map.Strict qualified as Map
import UnliftIO

import Lamagraph.Compiler.MonadFresh
import Lamagraph.Compiler.Nets.Reduction
import Lamagraph.Compiler.Nets.Types
import Lamagraph.Compiler.Nets.Utils

-------------------
-- Peano Example --
-------------------

data Peano = Zero | Succ | Add deriving (Show)

peanoRule :: (Rule Peano)
peanoRule leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  (Succ, Add) -> do
    xIn <- fresh
    resIn <- fresh
    xOut <- fresh
    resOut <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [AnnTerm [Just xIn, Just resOut, Nothing] (Agent Add [Var xIn, Var resOut])]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar xOut, AnnTerm [Just resIn, Nothing] (Agent Succ [Var resIn])]
    pure (rightPairs <> leftPairs, Map.fromList [(xIn, xOut), (xOut, xIn), (resOut, resIn), (resIn, resOut)])
  (Zero, Add) -> do
    xIn <- fresh
    xOut <- fresh
    let rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar xIn, mkAnnVar xOut]
    pure (rightPairs, Map.fromList [(xIn, xOut), (xOut, xIn)])
  (l, r) -> throwString $ "Cant reduce " <> show l <> " and " <> show r

peanoReduce :: Configuration Peano -> INsMachine (Configuration Peano)
peanoReduce = reduceStep peanoRule

peanoNet1 :: Net Peano
peanoNet1 = Net{equations = [(Agent Succ [Agent Zero []], Agent Add [Var 1, Var 2])], terms = [Var 1, Var 2]}

peanoConf1 :: INsMachine (Configuration Peano)
peanoConf1 = do
  xIn <- fresh
  resOut <- fresh
  resIn <- fresh
  xOut <- fresh
  pure $
    mkConfigurationWithDefault
      [
        ( AnnTerm [Nothing] (Agent Succ [Agent Zero []])
        , AnnTerm [Just xIn, Just resOut, Nothing] (Agent Add [Var xIn, Var resOut])
        )
      ]
      (Map.fromList [(xIn, xOut), (xOut, xIn), (resOut, resIn), (resIn, resOut)])
      [mkAnnVar resIn, mkAnnVar xOut]

peanoNet2 :: Net Peano
peanoNet2 = Net{equations = [(Agent Succ [Agent Zero []], Agent Add [Agent Zero [], Var 1])], terms = [Var 1]}

peanoConf2 :: INsMachine (Configuration Peano)
peanoConf2 = do
  xOut <- fresh
  xIn <- fresh
  pure $
    mkConfigurationWithDefault
      [(AnnTerm [Nothing] (Agent Succ [Agent Zero []]), AnnTerm [Just xOut, Nothing] (Agent Add [Agent Zero [], Var xOut]))]
      (Map.fromList [(xIn, xOut), (xOut, xIn)])
      [mkAnnVar xIn]

-------------------
-- Linear lambda --
-------------------

data LLambda = App | Abs deriving (Show)

lLambdaRule :: Rule LLambda
lLambdaRule leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  (App, Abs) -> do
    x1 <- fresh
    y1 <- fresh
    x2 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1, mkAnnVar x2]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar y1, mkAnnVar y2]
    pure (rightPairs <> leftPairs, Map.fromList [(x1, y1), (y1, x1), (x2, y2), (y2, x2)])
  (Abs, App) -> lLambdaRule rightLabel rightAux leftLabel leftAux
  (l, r) -> throwString $ "Cant reduce " <> show l <> " and " <> show r

lLambdaReduce :: Configuration LLambda -> INsMachine (Configuration LLambda)
lLambdaReduce = reduceStep lLambdaRule

lLambdaNet1 :: Net LLambda
lLambdaNet1 = Net{terms = [Var 1], equations = [(Agent Abs [Var 2, Var 2], Agent App [Var 1, Agent Abs [Var 3, Var 3]])]}

lLambdaConf1 :: INsMachine (Configuration LLambda)
lLambdaConf1 = do
  x1 <- fresh
  x2 <- fresh
  y1 <- fresh
  y2 <- fresh
  resOut <- fresh
  resIn <- fresh
  pure $
    mkConfigurationWithDefault
      [
        ( AnnTerm [Just x1, Just x2, Nothing] (Agent Abs [Var x1, Var x2])
        , AnnTerm [Just y1, Just y2, Just resOut, Nothing] (Agent App [Var resOut, Agent Abs [Var y2, Var y1]])
        )
      ]
      (Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1), (resIn, resOut), (resOut, resIn)])
      [mkAnnVar resIn]
