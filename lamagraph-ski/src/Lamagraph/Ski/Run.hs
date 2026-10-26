{- | A pure, step-bounded reduction driver for SKI interaction nets. It
applies the SKI rule set until no rule fires (normal form) or the step
budget is exhausted. No IO and no per-step printing, so it is suitable for
property tests.
-}
module Lamagraph.Ski.Run (
  MaxSteps (..),
  StepOverflow (..),
  defaultMaxSteps,
  reduceToNormalForm,
  reduceSteps,
  reduceStepsFixedOrder,
  applyOneReport,
  showGraph,
) where

import Relude

import Data.List qualified as List
import GraphRewriting (
  Graph,
  Rule,
  apply',
  edges,
  nodes,
  runGraph,
 )
import INet.Graph (SKI (..))
import INet.Rules qualified as Rules
import Lamagraph.Ski.Worklist (reduceStepsWL)

{- | A step budget bounding SKI reduction. Wraps the raw step count so a step
budget is not confused with a step count or a plain integer.
-}
newtype MaxSteps = MaxSteps Int
  deriving (Eq, Ord, Show)

{- | The step count at which a reduction exhausted its
'Lamagraph.Ski.Run.MaxSteps' budget before reaching normal form.
-}
newtype StepOverflow = StepOverflow Int
  deriving (Eq, Ord, Show)

-- | The default step budget for reducing a SKI net to normal form.
defaultMaxSteps :: MaxSteps
defaultMaxSteps = MaxSteps 100000

-- | The full SKI reduction rule set.
skiRules :: [Rule SKI]
skiRules =
  [ Rules.ruleI
  , Rules.ruleK0
  , Rules.ruleK1
  , Rules.ruleS0
  , Rules.ruleS1
  , Rules.ruleS2
  , Rules.ruleE0
  , Rules.ruleE1
  , Rules.ruleE2
  , Rules.ruleD0
  , Rules.ruleD1
  , Rules.ruleD2
  , Rules.eliminate
  ]

{- | Reduce a SKI graph to normal form, or return 'Left' with the step count at
which the 'Lamagraph.Ski.Run.MaxSteps' budget was exhausted.
-}
reduceToNormalForm :: MaxSteps -> Graph SKI -> Either StepOverflow (Graph SKI)
reduceToNormalForm (MaxSteps maxSteps) g0 =
  case reduceSteps maxSteps g0 of
    (_, g, True) -> Right g
    (step, _, False) -> Left (StepOverflow step)

{- | Reduce a SKI graph for at most @n@ steps using the worklist driver
('Lamagraph.Ski.Worklist.reduceStepsWL'). Returns the step count reached, the
resulting graph, and whether normal form was reached (no rule fires).
-}
reduceSteps :: Int -> Graph SKI -> (Int, Graph SKI, Bool)
reduceSteps = reduceStepsWL

{- | Reduce a SKI graph for at most @n@ steps with the fixed-order driver: on
every step it tries each rule (in a fixed order) against the whole graph. This
is the reference implementation — correct but O(graph size) per step — kept for
debugging and for verifying the worklist driver against it.
-}
reduceStepsFixedOrder :: Int -> Graph SKI -> (Int, Graph SKI, Bool)
reduceStepsFixedOrder maxSteps g0 = go 0 g0 maxSteps
 where
  go step g remaining
    | remaining <= 0 = (step, g, False)
    | otherwise = case applyOne g of
        (True, g') -> go (step + 1) g' (remaining - 1)
        (False, _) -> (step, g, True)

-- | Names of the SKI rules, parallel to 'skiRules' (for debugging).
ruleNames :: [String]
ruleNames =
  [ "I"
  , "K0"
  , "K1"
  , "S0"
  , "S1"
  , "S2"
  , "E0"
  , "E1"
  , "E2"
  , "D0"
  , "D1"
  , "D2"
  , "eliminate"
  ]

{- | Apply the first rule (in a fixed order) that fires; report whether any
rule fired and the resulting graph.
-}
applyOne :: Graph SKI -> (Bool, Graph SKI)
applyOne g0 = go g0 skiRules
 where
  go _ [] = (False, g0)
  go gcur (r : rs) =
    let (fired, g') = runGraph (apply' r) gcur
     in if fired then (True, g') else go gcur rs

-- | Apply one step, reporting which rule fired (if any). For debugging.
applyOneReport :: Graph SKI -> (Maybe String, Graph SKI)
applyOneReport g0 = go g0 (zip ruleNames skiRules)
 where
  go _ [] = (Nothing, g0)
  go gcur ((name, r) : rs) =
    let (fired, g') = runGraph (apply' r) gcur
     in if fired then (Just name, g') else go gcur rs

{- | A human-readable dump of a SKI graph (its nodes and the node sets each
edge connects). For debugging / the S2 spike.
-}
showGraph :: Graph SKI -> String
showGraph g =
  List.unlines
    ( "Nodes (" <> show (length (nodes g)) <> "):"
        : map (("  node " <>) . show) (nodes g)
          <> ["Edges (" <> show (length (edges g)) <> "):"]
          <> map (showEdge . snd) (edges g)
    )
 where
  showEdge :: [SKI] -> String
  showEdge ns = "  edge -> " <> show (map (show :: SKI -> String) ns)
