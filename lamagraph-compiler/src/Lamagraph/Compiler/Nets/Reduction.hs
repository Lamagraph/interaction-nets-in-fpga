-- | Module with main logic for abstract machine
module Lamagraph.Compiler.Nets.Reduction (reduce, reduceStep) where

import Relude

import Control.Monad.Extra (loopM)

import Data.Map.Strict qualified as Map
import UnliftIO

import Lamagraph.Compiler.Nets.Types
import Lamagraph.Compiler.Nets.Utils

------------------------------
-- Main reduction functions --
------------------------------

-- This function dispatches rules I-III.5 from Table 1 in [2].
-- Function is written in very imperative way: we have many helpers with names like @process<RULE_NAME>@.
-- If rule fires, then it returns Just with new configuration, otherwise it returns Nothing and next rule tries to fire.
-- If no more rules left, by propositions in [2], we know that we have a degenerate configuration and throw an exception.
process ::
  (Show label, Typeable label) =>
  Rule label ->
  Configuration label ->
  (AnnTerm label, AnnTerm label) ->
  INsMachine (Configuration label)
process rule conf@Configuration{phi} = \case
  (AnnTerm _ (Agent leftLabel leftAux), AnnTerm _ (Agent rightLabel rightAux)) -> do
    -- I
    (newStackTop, newPhiTop) <- rule leftLabel leftAux rightLabel rightAux
    pure conf{phi = newPhiTop <> phi, threadState = Enlist newStackTop}
  (AnnTerm _ (Var x), AnnTerm _ (Var y)) ->
    processII1 conf x y -- II.1
      `whenNothing` processII2 conf x y -- II.2
      `whenNothing` processII2 conf y x -- II.2 (symmetric)
      `whenNothing` processII3 conf x y -- II.3
      `whenNothing` processII3 conf y x -- II.3 (symmetric)
      `whenNothing` processII4 conf x y -- II.4
      `whenNothing` throwIO (CannotApplyAnyRuleException conf)
  (agent@(AnnTerm _ (Agent{})), var@(AnnTerm _ (Var _))) -> pure $ conf{threadState = Process (var, agent)} -- III.0
  (AnnTerm _ (Var z), AnnTerm ((Just y) : tBig) alpha@(Agent{})) ->
    processIII1 conf z y tBig alpha -- III.1
      `whenNothing` processIII2 conf z y tBig alpha -- III.2
      `whenNothing` processIII3 conf z y tBig alpha -- III.3
      `whenNothing` throwIO (CannotApplyAnyRuleException conf)
  (AnnTerm _ (Var z), AnnTerm (Nothing : tBig) alpha@(Agent{})) ->
    processIII4 conf z tBig alpha -- III.4
      `whenNothing` processIII5 conf z tBig alpha -- III.5
      `whenNothing` throwIO (CannotApplyAnyRuleException conf)
  (AnnTerm _ (Var _), AnnTerm [] (Agent _ _)) -> throwIO (CannotApplyAnyRuleException conf)

{- | One step of the abstract machine.

Machine's control flow is based on Table 1 from [2] (see "Lamagraph.Compiler.Nets.Types").
-}
reduceStep :: (Show label, Typeable label) => Rule label -> Configuration label -> INsMachine (Configuration label)
-- This function implements rules T.1-4 from Table 1 in [2]
reduceStep rule conf@Configuration{stack, cycles, threadState} = case threadState of
  -- T.1
  Delist -> case stack of
    [] -> throwIO $ DelistFromEmptyStackException conf
    hd : tl -> pure $ conf{stack = tl, threadState = Process hd}
  -- T.2
  Enlist (hd : tl) -> pure $ conf{stack = hd : stack, threadState = Enlist tl}
  -- T.3
  Enlist [] -> pure $ conf{threadState = Delist}
  -- T.4
  Cycle newCycle -> pure $ conf{cycles = uncurry Map.insert newCycle cycles}
  -- Main processing
  Process pair -> process rule conf pair

-- | Looping version of 'reduceStep'. Works until 'stack' is empty and 'threadState' is 'Delist'.
reduce ::
  (Show label, Typeable label) => Rule label -> Configuration label -> INsMachine (Configuration label)
reduce rule = loopM helper
 where
  helper conf@Configuration{stack, threadState} = case (stack, threadState) of
    ([], Delist) -> pure $ Right conf
    _ -> do
      newConf <- reduceStep rule conf
      pure $ Left newConf

-----------------------
-- Reduction helpers --
-----------------------

-- Names of these functions correspond to the rules in the Table 1 in [2]

processII1 :: Configuration label -> Var -> Var -> Maybe (Configuration label)
processII1 conf@Configuration{phi} x y =
  if memberKeyValue phi x y
    then Just conf{threadState = Cycle (x, mkAnnVar y)}
    else Nothing

processII2 :: Configuration label -> Var -> Var -> Maybe (Configuration label)
processII2 conf@Configuration{heap, phi} x y =
  case Map.lookup x phi of
    Just z -> case Map.lookup z heap of
      Just sigma ->
        Just
          conf
            { heap = Map.delete z heap
            , phi = Map.delete z $ Map.delete x phi
            , threadState = Process (sigma, mkAnnVar y)
            }
      Nothing -> Nothing
    Nothing -> Nothing

processII3 :: Configuration label -> Var -> Var -> Maybe (Configuration label)
processII3 conf@Configuration{heap, phi} x y = case (Map.lookup x phi, Map.lookup y phi) of
  (Just z, Just w) -> case (Map.notMember z heap, Map.lookup w heap) of
    (True, Just tau) ->
      Just
        conf
          { heap = Map.delete w heap
          , phi = Map.delete y $ Map.delete w phi
          , threadState = Process (mkAnnVar x, tau)
          }
    _ -> Nothing
  _ -> Nothing

processII4 :: Configuration label -> Var -> Var -> Maybe (Configuration label)
processII4 conf@Configuration{heap, phi} x y = case (Map.lookup x phi, Map.lookup y phi) of
  (Just z, Just w) ->
    if (y /= z) && (x /= w) && Map.notMember z heap && Map.notMember w heap
      then
        Just
          conf
            { phi = Map.insert z w $ Map.insert w z $ Map.delete x $ Map.delete z $ Map.delete y $ Map.delete w phi
            , threadState = Delist
            }
      else Nothing
  _ -> Nothing

processIII1 ::
  Configuration label -> Var -> Var -> [Maybe Var] -> Term label -> Maybe (Configuration label)
processIII1 conf@Configuration{heap, phi} z y tBig alpha = case Map.lookup y phi of
  Just x -> case Map.lookup x heap of
    Just (AnnTerm xBig beta) ->
      let newTerm = AnnTerm (deleteFirstNothing xBig ++ tBig) (substitute y beta alpha)
       in Just
            conf
              { heap = Map.delete x heap
              , phi = Map.delete x $ Map.delete y phi
              , threadState = Process (mkAnnVar z, newTerm)
              }
    Nothing -> Nothing
  Nothing -> Nothing

processIII2 ::
  Configuration label -> Var -> Var -> [Maybe Var] -> Term label -> Maybe (Configuration label)
processIII2 conf@Configuration{heap, phi} z y tBig alpha = case Map.lookup y phi of
  Just x ->
    if Map.notMember x heap && x /= z
      then Just conf{threadState = Process (mkAnnVar z, AnnTerm (tBig <> [Just y]) alpha)}
      else Nothing
  Nothing -> Nothing

processIII3 ::
  Configuration label -> Var -> Var -> [Maybe Var] -> Term label -> Maybe (Configuration label)
processIII3 conf@Configuration{phi} x y tBig alpha =
  if memberKeyValue phi x y
    then Just conf{threadState = Cycle (x, AnnTerm (Just y : tBig) alpha)}
    else Nothing

processIII4 :: Configuration label -> Var -> [Maybe Var] -> Term label -> Maybe (Configuration label)
processIII4 conf@Configuration{heap, phi} z tBig alpha = case Map.lookup z phi of
  Just x -> case Map.lookup x heap of
    Just betaAnn@(AnnTerm _ (Agent{})) ->
      Just
        conf
          { heap = Map.delete x heap
          , phi = Map.delete x $ Map.delete z phi
          , threadState = Process (betaAnn, AnnTerm (Nothing : tBig) alpha)
          }
    _ -> Nothing
  Nothing -> Nothing

processIII5 :: Configuration label -> Var -> [Maybe Var] -> Term label -> Maybe (Configuration label)
processIII5 conf@Configuration{heap, phi} z tBig alpha = case Map.lookup z phi of
  Just x ->
    if Map.notMember x heap
      then Just conf{heap = Map.insert z (AnnTerm (tBig <> [Nothing]) alpha) heap, threadState = Delist}
      else Nothing
  Nothing -> Nothing
