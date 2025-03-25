-- | Pre- and postprocessing functions for abstract machine
module Lamagraph.Compiler.Nets.Processing (netToConfiguration, updateStep, update, configurationToNet) where

import Relude hiding (atomically, newTVarIO, readTVarIO)
import Relude.Extra.Bifunctor

import Control.Monad.Extra (loopM)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Max (..))
import Data.Traversable (mapAccumM)
import UnliftIO

import Lamagraph.Compiler.MonadFresh
import Lamagraph.Compiler.Nets.Types
import Lamagraph.Compiler.Nets.Utils

--------------------------
-- Net to Configuration --
--------------------------

{- | Function for converting 'Net' to 'INsMachine' 'Configuration'.

This function does the following work:

1. Checks that given that is valid, thus every variable occurs exactly twice.
2. Sets the internal counter of 'INsMachine' to the one above maximum value of variable in the net .
3. Splits variables--for every variable \(x\) generates \(x'\) and adds \(x \leftrightarrow x'\) to \(\phi_N\).
4. Annotates all terms.
5. Produces 'Configuration'.
-}
netToConfiguration :: (Show label, Typeable label) => Net label -> INsMachine (Configuration label)
netToConfiguration net = do
  isNetValid net
  updateCounter net
  (phi, splitNet) <- splitVars Map.empty net
  let (iface, stack) = annotateNet splitNet
  pure $ mkConfigurationWithDefault stack phi iface

isNetValid :: (Show label, Typeable label) => Net label -> INsMachine ()
isNetValid net@Net{terms, equations} = do
  let counterAfterTerms = foldl' countVars Map.empty terms
      counterAfterEquations = foldl' (\acc (l, r) -> countVars (countVars acc l) r) counterAfterTerms equations
      invalidVars = Map.filter (/= 2) counterAfterEquations
  if null invalidVars then pure () else throwIO $ IncorrectNet net (Map.toList invalidVars)
 where
  countVars :: Map Var Int -> Term label -> Map Var Int
  countVars counter = \case
    (Var var) -> if Map.member var counter then Map.adjust (+ 1) var counter else Map.insert var 1 counter
    Agent _ aux -> foldl' countVars counter aux

updateCounter :: Net label -> INsMachine ()
updateCounter net = do
  env <- ask
  let newFreshCounter = findMaxVar net + 1
  atomically $ writeTVar (getFreshCounter env) newFreshCounter

findMaxVar :: Net label -> Int
findMaxVar Net{terms, equations} =
  getMax $ foldMap findMaxVarInTerm terms <> foldMap (\(t1, t2) -> findMaxVarInTerm t1 <> findMaxVarInTerm t2) equations

findMaxVarInTerm :: Term label -> Max Var
findMaxVarInTerm = \case
  Var var -> Max var
  Agent _ ports -> foldMap findMaxVarInTerm ports

splitVars :: Map Var Var -> Net label -> INsMachine (Map Var Var, Net label)
splitVars phi Net{terms, equations} = do
  (newPhi, splitTerms) <- mapAccumM splitVarsHelper phi terms
  (newPhi', splitEquations) <- mapAccumM helper newPhi equations
  pure (newPhi', Net{terms = splitTerms, equations = splitEquations})
 where
  helper helperPhi (leftTerm, rightTerm) = do
    (leftPhi, splitLeftTerm) <- splitVarsHelper helperPhi leftTerm
    (rightPhi, splitRightTerm) <- splitVarsHelper leftPhi rightTerm
    pure (rightPhi, (splitLeftTerm, splitRightTerm))

splitVarsHelper :: Map Var Var -> Term label -> INsMachine (Map Var Var, Term label)
splitVarsHelper phi = \case
  Var var -> do
    case Map.lookup var phi of
      Just newVar -> pure (phi, Var newVar)
      Nothing -> do
        newVar <- fresh
        pure (Map.insert var newVar $ Map.insert newVar var phi, Var var)
  Agent label aux -> do
    (newPhi, splitAux) <- mapAccumM splitVarsHelper phi aux
    pure (newPhi, Agent label splitAux)

annotateNet :: Net label -> ([AnnTerm label], [(AnnTerm label, AnnTerm label)])
annotateNet Net{terms, equations} = (map annotateTerm terms, map (bimapBoth annotateTerm) equations)

--------------
-- Updating --
--------------

{- | Updates 'Configuration' in by the rules from Definition 3 from [2] (see "Lamagraph.Compiler.Nets.Types") for converting back to 'Net'.
One-step version.
-}
updateStep :: (Typeable label, Show label) => Configuration label -> INsMachine (Configuration label)
updateStep conf@Configuration{heap, stack, phi, iface, cycles, threadState} = case Map.lookupMin heap of
  Just (phiX, phiXTerm) -> case Map.lookup phiX phi of
    Just x ->
      pure
        conf
          { heap = Map.delete phiX heap
          , stack = map (bimapBoth $ substituteAnnTerm x phiXTerm) stack
          , phi = Map.delete x $ Map.delete phiX phi
          , iface = map (substituteAnnTerm x phiXTerm) iface
          , cycles = Map.map (substituteAnnTerm x phiXTerm) cycles
          , threadState = threadStateHelper x phiXTerm threadState
          }
    Nothing -> throwIO $ DegenerateConfigurationException conf
  Nothing -> pure conf

-- | Looping version of 'updateStep'. Works until 'heap' is empty.
update :: (Typeable label, Show label) => Configuration label -> INsMachine (Configuration label)
update = loopM helper
 where
  helper conf@Configuration{heap} = if Map.null heap then pure $ Right conf else Left <$> updateStep conf

threadStateHelper :: Var -> AnnTerm label -> ThreadState label -> ThreadState label
threadStateHelper x phiXTerm = \case
  Process pair -> Process $ bimapBoth (substituteAnnTerm x phiXTerm) pair
  Enlist pairs -> Enlist $ map (bimapBoth $ substituteAnnTerm x phiXTerm) pairs
  Delist -> Delist
  -- Case var == x cannot happen, otherwise we must have an active pair that can be reduced
  Cycle (var, term) -> Cycle (var, substituteAnnTerm x phiXTerm term)

---------------------------
-- Configuration to Nets --
---------------------------

-- | Function for transforming 'Configuration' back to 'Net' using Definitions 4 & 5 from [2].
configurationToNet :: (Show label, Typeable label) => Configuration label -> INsMachine (Net label)
configurationToNet conf@Configuration{heap, stack, iface, cycles, threadState} =
  if null heap
    then
      let strippedTerms = map unAnnotateTerm iface
          strippedEquations =
            map (bimapBoth unAnnotateTerm) stack <> map (bimap Var unAnnotateTerm) (Map.toList cycles) <> unThreadState threadState
       in pure Net{terms = map (collapseVar conf) strippedTerms, equations = map (bimapBoth (collapseVar conf)) strippedEquations}
    else throwIO $ DegenerateConfigurationException conf

unThreadState :: ThreadState label -> [(Term label, Term label)]
unThreadState = \case
  Delist -> []
  Process (leftTerm, rightTerm) -> [(unAnnotateTerm leftTerm, unAnnotateTerm rightTerm)]
  Enlist pairs -> map (bimapBoth unAnnotateTerm) pairs
  Cycle (var, term) -> [(Var var, unAnnotateTerm term)]

collapseVar :: (Show label, Typeable label) => Configuration label -> Term label -> Term label
collapseVar conf@Configuration{phi} = \case
  Var x -> case Map.lookup x phi of
    Just y -> if x < y then Var x else Var y
    Nothing -> impureThrow $ DegenerateConfigurationException conf
  Agent label aux -> Agent label $ map (collapseVar conf) aux
