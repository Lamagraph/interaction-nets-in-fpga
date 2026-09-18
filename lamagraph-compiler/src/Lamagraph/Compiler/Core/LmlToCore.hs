module Lamagraph.Compiler.Core.LmlToCore (desugarLmlModule) where

import Relude

import Control.Monad.Extra
import Data.Foldable.Extra hiding (elem)
import Data.List (partition, (!!))
import Data.List.NonEmpty qualified as NE

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.TcTypes

desugarLmlLit :: LmlLit LmlcTc -> Literal
desugarLmlLit = \case
  LmlInt _ int -> LitInt int
  LmlChar _ char -> LitChar char
  LmlString _ string -> LitString string

desugarLLmlExpr :: LLmlExpr LmlcTc -> MonadDesugar CoreExpr
desugarLLmlExpr (L _ expr) = desugarLmlExpr expr

desugarLmlExpr :: LmlExpr LmlcTc -> MonadDesugar CoreExpr
desugarLmlExpr = \case
  LmlExprIdent _ longident -> pure $ Var $ Id $ Name longident
  LmlExprConstant _ lit -> pure $ Lit $ desugarLmlLit lit
  LmlExprLet _ lBindGroup lExpr -> foldr Let <$> desugarLLmlExpr lExpr <*> desugarLLmlBindGroup lBindGroup
  LmlExprFunction _ lPat lExpr -> Lam <$> desugarLLmlPat lPat <*> desugarLLmlExpr lExpr
  LmlExprApply _ lExpr lExprs -> foldl App <$> desugarLLmlExpr lExpr <*> mapM desugarLLmlExpr lExprs
  LmlExprMatch _ lExpr lCases -> do
    scrutineeVar <- freshVar
    expr <- desugarLLmlExpr lExpr
    cases <- compilePatterns scrutineeVar lCases
    pure $ Match expr scrutineeVar cases
  LmlExprTuple _ lExpr lExprs -> Tuple <$> desugarLLmlExpr lExpr <*> mapM desugarLLmlExpr lExprs
  LmlExprConstruct _ (L _ longident) maybeArgs ->
    let constructorVar = Var $ Id $ Name longident
     in case maybeArgs of
          Nothing -> pure constructorVar
          Just lArgs -> App constructorVar <$> desugarLLmlExpr lArgs
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    trueExpr <- desugarLLmlExpr lTrue
    falseExpr <- desugarLLmlExpr lFalse
    let trueAlt = (DataAlt trueConstrName, [], trueExpr)
        falseAlt = (DataAlt falseConstrName, [], falseExpr)
    condExpr <- desugarLLmlExpr lCond
    var <- freshVar
    pure $ Match condExpr var (trueAlt :| [falseAlt])
  LmlExprConstraint _ lExpr _ -> desugarLLmlExpr lExpr

expandOrPatterns :: NonEmpty (LLmlCase LmlcTc) -> NonEmpty (LLmlCase LmlcTc)
expandOrPatterns cases =
  let expanded = concatMap expandCase (toList cases)
   in fromMaybe cases (nonEmpty expanded)
 where
  expandCase :: LLmlCase LmlcTc -> [LLmlCase LmlcTc]
  expandCase (L loc (LmlCase ty pat guardExpr rhs)) =
    let expandedPats = expandPat pat
     in [L loc (LmlCase ty p guardExpr rhs) | p <- expandedPats]

  expandPat :: LLmlPat LmlcTc -> [LLmlPat LmlcTc]
  expandPat lPat@(L loc pat) = case pat of
    LmlPatOr _ p1 p2 ->
      expandPat p1 ++ expandPat p2
    LmlPatConstruct x name maybeSub ->
      case maybeSub of
        Nothing -> [lPat]
        Just sub ->
          let expandedSubs = expandPat sub
           in [L loc (LmlPatConstruct x name (Just s)) | s <- expandedSubs]
    LmlPatTuple x p1 ps ->
      let allPats = p1 : toList ps
          expandedPats = map expandPat allPats
          combinations = sequence expandedPats
          buildTuple (firstPat : rest) = case nonEmpty rest of
            Just restNE -> Just (L loc (LmlPatTuple x firstPat restNE))
            Nothing -> Nothing
          buildTuple [] = Nothing
       in mapMaybe buildTuple combinations
    LmlPatConstraint x p ty ->
      [L loc (LmlPatConstraint x expanded ty) | expanded <- expandPat p]
    _ -> [lPat]

compilePatterns :: Var -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar (NonEmpty CoreMatchAlt)
compilePatterns scrutineeVar lCases = do
  let expandedCases = expandOrPatterns lCases

  let hasGuards = any caseHasGuard expandedCases

  if hasGuards
    then compilePatternsWithBacktracking scrutineeVar expandedCases
    else compilePatternsOriginal scrutineeVar expandedCases

caseHasGuard :: LLmlCase LmlcTc -> Bool
caseHasGuard (L _ (LmlCase _ _ maybeGuard _)) = isJust maybeGuard

compilePatternsOriginal :: Var -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar (NonEmpty CoreMatchAlt)
compilePatternsOriginal scrut cases = do
  alts <- groupAndCompileCases scrut (toList cases)
  case nonEmpty alts of
    Just altsNE -> pure altsNE
    Nothing -> pure $ pure (DEFAULT, [], Var scrut)

groupAndCompileCases :: Var -> [LLmlCase LmlcTc] -> MonadDesugar [CoreMatchAlt]
groupAndCompileCases _ [] = pure []
groupAndCompileCases scrut (firstCase : restCases) = do
  let (sameGroup, different) = partition (sameOuterPatternNoGuard firstCase) restCases
      currentGroup = firstCase :| sameGroup

  compiledGroup <- compilePatternGroupOriginal scrut currentGroup
  compiledRest <- groupAndCompileCases scrut different

  pure (compiledGroup : compiledRest)

sameOuterPatternNoGuard :: LLmlCase LmlcTc -> LLmlCase LmlcTc -> Bool
sameOuterPatternNoGuard (L _ (LmlCase _ (L _ pat1) _ _)) (L _ (LmlCase _ (L _ pat2) _ _)) =
  samePat pat1 pat2
 where
  samePat :: LmlPat LmlcTc -> LmlPat LmlcTc -> Bool
  samePat (LmlPatConstruct _ (L _ name1) _) (LmlPatConstruct _ (L _ name2) _) = name1 == name2
  samePat (LmlPatAny _) (LmlPatAny _) = True
  samePat (LmlPatVar{}) (LmlPatVar{}) = True
  samePat (LmlPatVar{}) (LmlPatAny _) = True
  samePat (LmlPatAny _) (LmlPatVar{}) = True
  samePat (LmlPatTuple{}) (LmlPatTuple{}) = True
  samePat (LmlPatConstant _ lit1) (LmlPatConstant _ lit2) = sameLiteral lit1 lit2
  samePat _ _ = False

  sameLiteral :: LmlLit LmlcTc -> LmlLit LmlcTc -> Bool
  sameLiteral (LmlInt _ n1) (LmlInt _ n2) = n1 == n2
  sameLiteral (LmlChar _ c1) (LmlChar _ c2) = c1 == c2
  sameLiteral (LmlString _ s1) (LmlString _ s2) = s1 == s2
  sameLiteral _ _ = False

compilePatternGroupOriginal :: Var -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar CoreMatchAlt
compilePatternGroupOriginal scrut (singleCase@(L _ (LmlCase ty (L _ pat) _ _)) :| []) =
  case pat of
    LmlPatConstruct _ (L _ constrName) maybeSub ->
      case maybeSub of
        Nothing -> desugarLLmlCase scrut singleCase
        Just _ -> compileConstructorGroup scrut ty (Name constrName) (singleCase :| [])
    LmlPatTuple{} ->
      compileTupleGroup scrut ty (singleCase :| [])
    _ ->
      desugarLLmlCase scrut singleCase
compilePatternGroupOriginal scrut cases@(firstCase :| _) =
  case firstCase of
    L _ (LmlCase ty (L _ firstPat) _ _) -> case firstPat of
      LmlPatConstruct _ (L _ constrName) _ ->
        compileConstructorGroup scrut ty (Name constrName) cases
      LmlPatTuple{} ->
        compileTupleGroup scrut ty cases
      LmlPatVar{} ->
        desugarLLmlCase scrut firstCase
      LmlPatAny _ ->
        desugarLLmlCase scrut firstCase
      _ ->
        desugarLLmlCase scrut firstCase

compilePatternsWithBacktracking :: Var -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar (NonEmpty CoreMatchAlt)
compilePatternsWithBacktracking scrutineeVar cases = do
  compiled <- compileCasesWithFallback scrutineeVar (toList cases) (Var scrutineeVar)

  case nonEmpty compiled of
    Just alts -> pure alts
    Nothing -> pure $ pure (DEFAULT, [], Var scrutineeVar)

compileCasesWithFallback :: Var -> [LLmlCase LmlcTc] -> CoreExpr -> MonadDesugar [CoreMatchAlt]
compileCasesWithFallback _ [] _fallback = pure []
compileCasesWithFallback scrut (firstCase : restCases) fallback = do
  let isDefaultCase (L _ (LmlCase _ (L _ pat) _ _)) = case pat of
        LmlPatVar{} -> True
        LmlPatAny _ -> True
        _ -> False
      isDefaultCase (L _ (XLmlCase _)) = False

  if isDefaultCase firstCase
    then do
      let allDefaultCases = firstCase : filter isDefaultCase restCases
          nonDefaultCases = filter (not . isDefaultCase) restCases

      nonDefaultAlts <-
        if null nonDefaultCases
          then pure []
          else compileCasesWithFallback scrut nonDefaultCases fallback

      defaultFallback <-
        if null nonDefaultCases
          then pure fallback
          else case nonEmpty nonDefaultAlts of
            Just alts -> pure $ Match (Var scrut) scrut alts
            Nothing -> pure fallback

      singleDefaultAlt <- compileDefaultCasesChain scrut allDefaultCases defaultFallback

      pure (singleDefaultAlt : nonDefaultAlts)
    else do
      let (sameGroup, different) = partition (sameOuterPattern firstCase) restCases
          currentGroup = firstCase :| sameGroup

      remainingAlts <-
        if null different
          then pure []
          else compileCasesWithFallback scrut different fallback

      let remainingFallback = case nonEmpty remainingAlts of
            Just alts -> Match (Var scrut) scrut alts
            Nothing -> fallback

      compiledGroup <- compilePatternGroupWithFallback scrut currentGroup remainingFallback

      pure (compiledGroup : remainingAlts)

compileDefaultCasesChain :: Var -> [LLmlCase LmlcTc] -> CoreExpr -> MonadDesugar CoreMatchAlt
compileDefaultCasesChain _scrut [] fallback =
  pure (DEFAULT, [], fallback)
compileDefaultCasesChain scrut (firstCase : restCases) fallback = do
  remainingFallback <-
    if null restCases
      then pure fallback
      else do
        (_, _, rhs) <- compileDefaultCasesChain scrut restCases fallback
        pure rhs

  (_, _, rhs) <- desugarLmlCaseWithFallback scrut firstCase remainingFallback

  pure (DEFAULT, [], rhs)

sameOuterPattern :: LLmlCase LmlcTc -> LLmlCase LmlcTc -> Bool
sameOuterPattern (L _ (LmlCase _ (L _ pat1) guard1 _)) (L _ (LmlCase _ (L _ pat2) guard2 _)) =
  isNothing guard1 && isNothing guard2 && samePat pat1 pat2
 where
  samePat :: LmlPat LmlcTc -> LmlPat LmlcTc -> Bool
  samePat (LmlPatConstruct _ (L _ name1) _) (LmlPatConstruct _ (L _ name2) _) = name1 == name2
  samePat (LmlPatTuple{}) (LmlPatTuple{}) = True
  samePat (LmlPatConstant _ lit1) (LmlPatConstant _ lit2) = sameLiteral lit1 lit2
  samePat _ _ = False

  sameLiteral :: LmlLit LmlcTc -> LmlLit LmlcTc -> Bool
  sameLiteral (LmlInt _ n1) (LmlInt _ n2) = n1 == n2
  sameLiteral (LmlChar _ c1) (LmlChar _ c2) = c1 == c2
  sameLiteral (LmlString _ s1) (LmlString _ s2) = s1 == s2
  sameLiteral _ _ = False

compilePatternGroupWithFallback :: Var -> NonEmpty (LLmlCase LmlcTc) -> CoreExpr -> MonadDesugar CoreMatchAlt
compilePatternGroupWithFallback scrut (singleCase@(L _ (LmlCase ty (L _ pat) _maybeGuard _)) :| []) fallback =
  case pat of
    LmlPatConstruct _ (L _ constrName) maybeSub ->
      case maybeSub of
        Nothing -> desugarLmlCaseWithFallback scrut singleCase fallback
        Just _ -> compileConstructorGroupWithFallback scrut ty (Name constrName) (singleCase :| []) fallback
    LmlPatTuple{} ->
      compileTupleGroupWithFallback scrut ty (singleCase :| []) fallback
    _ ->
      desugarLmlCaseWithFallback scrut singleCase fallback
compilePatternGroupWithFallback scrut cases@(firstCase :| _) fallback =
  case firstCase of
    L _ (LmlCase ty (L _ firstPat) _ _) -> case firstPat of
      LmlPatConstruct _ (L _ constrName) _ ->
        compileConstructorGroupWithFallback scrut ty (Name constrName) cases fallback
      LmlPatTuple{} ->
        compileTupleGroupWithFallback scrut ty cases fallback
      LmlPatVar{} ->
        desugarLmlCaseWithFallback scrut firstCase fallback
      LmlPatAny _ ->
        desugarLmlCaseWithFallback scrut firstCase fallback
      _ ->
        desugarLmlCaseWithFallback scrut firstCase fallback

compileConstructorGroup :: Var -> Ty -> Name -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar CoreMatchAlt
compileConstructorGroup scrut ty constrName cases@(firstCase :| _) = do
  let subPatternSets = NE.map extractConstructorSubPatterns cases

  if all isSimpleVarPattern (toList subPatternSets)
    then do
      desugarLLmlCase scrut firstCase
    else do
      compileNestedConstructorMatch scrut ty constrName cases subPatternSets

compileConstructorGroupWithFallback ::
  Var -> Ty -> Name -> NonEmpty (LLmlCase LmlcTc) -> CoreExpr -> MonadDesugar CoreMatchAlt
compileConstructorGroupWithFallback scrut ty constrName cases fallback = do
  let subPatternSets = NE.map extractConstructorSubPatterns cases
      firstCase = NE.head cases

  if all isSimpleVarPattern (toList subPatternSets)
    then desugarLmlCaseWithFallback scrut firstCase fallback
    else compileNestedConstructorMatchWithFallback scrut ty constrName (toList cases) (toList subPatternSets) fallback

extractConstructorSubPatterns :: LLmlCase LmlcTc -> Maybe (NonEmpty (LLmlPat LmlcTc))
extractConstructorSubPatterns (L _ (LmlCase _ (L _ (LmlPatConstruct _ _ maybeLPat)) _ _)) =
  case maybeLPat of
    Nothing -> Nothing
    Just (L _ (LmlPatTuple _ p1 ps)) -> Just (p1 :| toList ps)
    Just (L loc singlePat) -> Just (L loc singlePat :| [])
extractConstructorSubPatterns _ = Nothing

isSimpleVarPattern :: Maybe (NonEmpty (LLmlPat LmlcTc)) -> Bool
isSimpleVarPattern Nothing = True
isSimpleVarPattern (Just pats) = all isSimpleVar (toList pats)
 where
  isSimpleVar :: LLmlPat LmlcTc -> Bool
  isSimpleVar (L _ (LmlPatVar{})) = True
  isSimpleVar (L _ (LmlPatAny _)) = True
  isSimpleVar _ = False

compileNestedConstructorMatch ::
  Var ->
  Ty ->
  Name ->
  NonEmpty (LLmlCase LmlcTc) ->
  NonEmpty (Maybe (NonEmpty (LLmlPat LmlcTc))) ->
  MonadDesugar CoreMatchAlt
compileNestedConstructorMatch _scrut ty constrName cases (firstSet :| restSets) = do
  let arity = maybe 0 length firstSet

  argVars <- replicateM arity freshVar

  let rhsExprs = map extractRHS (toList cases)
      subPatternSets = firstSet : restSets

  nestedExpr <- buildNestedMatchForPosition argVars 0 ty (zip subPatternSets rhsExprs)

  pure (DataAlt constrName, argVars, nestedExpr)

compileNestedConstructorMatchWithFallback ::
  Var -> Ty -> Name -> [LLmlCase LmlcTc] -> [Maybe (NonEmpty (LLmlPat LmlcTc))] -> CoreExpr -> MonadDesugar CoreMatchAlt
compileNestedConstructorMatchWithFallback _scrut ty constrName cases subPatternSets fallback = do
  let arity = case listToMaybe subPatternSets of
        Nothing -> 0
        Just Nothing -> 0
        Just (Just pats) -> length pats

  argVars <- replicateM arity freshVar

  nestedExpr <- buildNestedMatchWithFallback argVars 0 ty cases subPatternSets fallback

  pure (DataAlt constrName, argVars, nestedExpr)

buildNestedMatchWithFallback ::
  [Var] -> Int -> Ty -> [LLmlCase LmlcTc] -> [Maybe (NonEmpty (LLmlPat LmlcTc))] -> CoreExpr -> MonadDesugar CoreExpr
buildNestedMatchWithFallback argVars pos ty cases subPatternSets fallback = do
  let casesWithPats = zip cases subPatternSets
      groupedByPattern = groupCasesBySubPatternAt pos casesWithPats

  alts <- mapM (compileSubPatternGroupWithFallback argVars pos ty fallback) groupedByPattern

  case nonEmpty alts of
    Nothing -> pure fallback
    Just altsNE -> do
      let scrutVar = argVars !! pos
      pure $ Match (Var scrutVar) scrutVar altsNE

groupCasesBySubPatternAt ::
  Int -> [(LLmlCase LmlcTc, Maybe (NonEmpty (LLmlPat LmlcTc)))] -> [[(LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc))]]
groupCasesBySubPatternAt pos casePairs =
  let extracted = map (second (getPatternAt pos)) casePairs
      grouped = groupByPatternType extracted
   in grouped
 where
  getPatternAt :: Int -> Maybe (NonEmpty (LLmlPat LmlcTc)) -> Maybe (LLmlPat LmlcTc)
  getPatternAt _ Nothing = Nothing
  getPatternAt idx (Just pats) = listToMaybe (drop idx (toList pats))

  groupByPatternType :: [(LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc))] -> [[(LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc))]]
  groupByPatternType [] = []
  groupByPatternType (p : ps) =
    let (same, diff) = partition (samePatType p) ps
     in (p : same) : groupByPatternType diff

  samePatType (_, Nothing) (_, Nothing) = True
  samePatType (_, Just (L _ p1)) (_, Just (L _ p2)) =
    case (p1, p2) of
      (LmlPatConstruct _ (L _ n1) _, LmlPatConstruct _ (L _ n2) _) -> n1 == n2
      (LmlPatVar{}, LmlPatVar{}) -> True
      (LmlPatAny _, LmlPatAny _) -> True
      (LmlPatVar{}, LmlPatAny _) -> True
      (LmlPatAny _, LmlPatVar{}) -> True
      _ -> False
  samePatType _ _ = False

  samePatType :: (LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc)) -> (LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc)) -> Bool

compileSubPatternGroupWithFallback ::
  [Var] -> Int -> Ty -> CoreExpr -> [(LLmlCase LmlcTc, Maybe (LLmlPat LmlcTc))] -> MonadDesugar CoreMatchAlt
compileSubPatternGroupWithFallback _argVars _pos _ty fallback [] =
  pure (DEFAULT, [], fallback)
compileSubPatternGroupWithFallback argVars pos _ty fallback ((firstCase, firstPat) : _rest) = case firstPat of
  Nothing -> do
    let scrutVar = argVars !! pos
    desugarLmlCaseWithFallback scrutVar firstCase fallback
  Just (L _ (LmlPatVar (FullName n, _) _)) -> do
    let (L _ (LmlCase _ _ maybeGuard rhs)) = firstCase
    rhsCore <- desugarLLmlExpr rhs

    finalRhs <- case maybeGuard of
      Nothing -> pure rhsCore
      Just guardExpr -> do
        guardResult <- desugarLLmlExpr guardExpr
        guardVar <- freshVar
        let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
            falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
        pure $ Match guardResult guardVar (trueAlt :| [falseAlt])

    let boundVar = Id $ Name n
    let scrutVar = argVars !! pos
    pure (DEFAULT, [], replaceVar boundVar scrutVar finalRhs)
  Just (L _ (LmlPatAny _)) -> do
    let (L _ (LmlCase _ _ maybeGuard rhs)) = firstCase
    rhsCore <- desugarLLmlExpr rhs

    finalRhs <- case maybeGuard of
      Nothing -> pure rhsCore
      Just guardExpr -> do
        guardResult <- desugarLLmlExpr guardExpr
        guardVar <- freshVar
        let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
            falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
        pure $ Match guardResult guardVar (trueAlt :| [falseAlt])

    pure (DEFAULT, [], finalRhs)
  Just (L _ (LmlPatConstruct _ (L _ constrName) maybeSub)) -> do
    case maybeSub of
      Nothing -> do
        let (L _ (LmlCase _ _ maybeGuard rhs)) = firstCase
        rhsCore <- desugarLLmlExpr rhs

        finalRhs <- case maybeGuard of
          Nothing -> pure rhsCore
          Just guardExpr -> do
            guardResult <- desugarLLmlExpr guardExpr
            guardVar <- freshVar
            let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
            pure $ Match guardResult guardVar (trueAlt :| [falseAlt])

        pure (DataAlt (Name constrName), [], finalRhs)
      Just subPat -> do
        let patternGroup = (firstCase, firstPat) : _rest
            allCasesInGroup = map fst patternGroup
            hasGuards = any (\(L _ (LmlCase _ _ g _)) -> isJust g) allCasesInGroup
            isComplexNesting = case subPat of
              L _ (LmlPatTuple _ p ps) -> not $ all isSimpleVarOrWildcard (p : toList ps)
              _ -> False

        let scrutVar = argVars !! pos
        if isComplexNesting && hasGuards
          then do
            let buildNestedCase (L caseLoc (LmlCase caseTy _ maybeGuard rhs), _) =
                  L caseLoc (LmlCase caseTy subPat maybeGuard rhs)
                nestedCases = map buildNestedCase patternGroup

            case nonEmpty nestedCases of
              Nothing -> do
                pure (DEFAULT, [], fallback)
              Just nestedCasesNE -> do
                nestedAlts <- compilePatternsWithBacktracking scrutVar nestedCasesNE
                pure (NE.head nestedAlts)
          else do
            let (L caseLoc (LmlCase caseTy _ maybeGuard rhs)) = firstCase
                newCase = L caseLoc (LmlCase caseTy subPat maybeGuard rhs)
            desugarLmlCaseWithFallback scrutVar newCase fallback
  Just subPat -> do
    let (L caseLoc (LmlCase caseTy _ maybeGuard rhs)) = firstCase
        newCase = L caseLoc (LmlCase caseTy subPat maybeGuard rhs)
    let scrutVar = argVars !! pos
    desugarLmlCaseWithFallback scrutVar newCase fallback

extractRHS :: LLmlCase LmlcTc -> LLmlExpr LmlcTc
extractRHS (L _ (LmlCase _ _ _ rhs)) = rhs

buildNestedMatchForPosition ::
  [Var] -> Int -> Ty -> [(Maybe (NonEmpty (LLmlPat LmlcTc)), LLmlExpr LmlcTc)] -> MonadDesugar CoreExpr
buildNestedMatchForPosition argVars pos ty casePairs = do
  let groupedByPattern = groupCasesBySubPattern pos casePairs

  alts <-
    Relude.mapMaybeM
      ( \grp -> case nonEmpty grp of
          Nothing -> pure Nothing
          Just groupNE -> Just <$> compileSubPatternGroup argVars pos ty groupNE
      )
      groupedByPattern

  case nonEmpty alts of
    Nothing -> do
      let scrutVar = argVars !! pos
      pure $ Var scrutVar
    Just altsNE -> do
      let scrutVar = argVars !! pos
      pure $ Match (Var scrutVar) scrutVar altsNE

groupCasesBySubPattern ::
  Int -> [(Maybe (NonEmpty (LLmlPat LmlcTc)), LLmlExpr LmlcTc)] -> [[(Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc)]]
groupCasesBySubPattern pos cases =
  let extracted = map (first (getPatternAt pos)) cases
      grouped = groupByPattern extracted
   in grouped
 where
  getPatternAt :: Int -> Maybe (NonEmpty (LLmlPat LmlcTc)) -> Maybe (LLmlPat LmlcTc)
  getPatternAt _ Nothing = Nothing
  getPatternAt idx (Just pats) = listToMaybe (drop idx (toList pats))

  groupByPattern :: [(Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc)] -> [[(Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc)]]
  groupByPattern [] = []
  groupByPattern (p : ps) =
    let (same, diff) = partition (samePatternType p) ps
     in (p : same) : groupByPattern diff

  samePatternType (Nothing, _) (Nothing, _) = True
  samePatternType (Just (L _ p1), _) (Just (L _ p2), _) =
    case (p1, p2) of
      (LmlPatConstruct _ (L _ n1) _, LmlPatConstruct _ (L _ n2) _) -> n1 == n2
      (LmlPatVar{}, LmlPatVar{}) -> True
      (LmlPatAny _, LmlPatAny _) -> True
      (LmlPatVar{}, LmlPatAny _) -> True
      (LmlPatAny _, LmlPatVar{}) -> True
      _ -> False
  samePatternType _ _ = False

  samePatternType :: (Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc) -> (Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc) -> Bool

compileSubPatternGroup ::
  [Var] -> Int -> Ty -> NonEmpty (Maybe (LLmlPat LmlcTc), LLmlExpr LmlcTc) -> MonadDesugar CoreMatchAlt
compileSubPatternGroup argVars pos ty patternGroup@((firstPat, firstRHS) :| _) = case firstPat of
  Nothing -> do
    rhs <- desugarLLmlExpr firstRHS
    pure (DEFAULT, [], rhs)
  Just (L _ (LmlPatVar (FullName n, _) _)) -> do
    rhs <- desugarLLmlExpr firstRHS
    let boundVar = Id $ Name n
    let scrutVar = argVars !! pos
    pure (DEFAULT, [], replaceVar boundVar scrutVar rhs)
  Just (L _ (LmlPatAny _)) -> do
    rhs <- desugarLLmlExpr firstRHS
    pure (DEFAULT, [], rhs)
  Just (L _ (LmlPatConstruct _ (L _ constrName) maybeSub)) -> do
    case maybeSub of
      Nothing -> do
        rhs <- desugarLLmlExpr firstRHS
        pure (DataAlt (Name constrName), [], rhs)
      Just (L _ subPat) -> do
        let subPatterns = case subPat of
              LmlPatTuple _ p ps -> p : toList ps
              otherPat -> [L generatedSrcSpan otherPat]
            arity = length subPatterns

        subVars <- replicateM arity freshVar

        let extractNestedPat (Just (L _ (LmlPatConstruct _ _ (Just (L _ constructorArg)))), rhs) =
              let subs = case constructorArg of
                    LmlPatTuple _ p ps -> Just $ p :| toList ps
                    otherPat -> Just $ L generatedSrcSpan otherPat :| []
               in (subs, rhs)
            extractNestedPat (Just (L _ (LmlPatConstruct _ _ Nothing)), rhs) =
              (Nothing, rhs)
            extractNestedPat (_, rhs) = (Nothing, rhs)
        let nestedPatterns = NE.map extractNestedPat patternGroup

        case subVars of
          [] -> do
            rhs <- desugarLLmlExpr firstRHS
            pure (DataAlt (Name constrName), [], rhs)
          _ : _ -> do
            nestedExpr <- buildNestedMatchForPosition subVars 0 ty (toList nestedPatterns)
            pure (DataAlt (Name constrName), subVars, nestedExpr)
  Just (L _ (LmlPatConstant _ lit)) -> do
    rhs <- desugarLLmlExpr firstRHS
    pure (LitAlt (desugarLmlLit lit), [], rhs)
  _ -> do
    rhs <- desugarLLmlExpr firstRHS
    pure (DEFAULT, [], rhs)

compileTupleGroup :: Var -> Ty -> NonEmpty (LLmlCase LmlcTc) -> MonadDesugar CoreMatchAlt
compileTupleGroup scrut _ty (firstCase :| _) =
  desugarLLmlCase scrut firstCase

compileTupleGroupWithFallback :: Var -> Ty -> NonEmpty (LLmlCase LmlcTc) -> CoreExpr -> MonadDesugar CoreMatchAlt
compileTupleGroupWithFallback scrut _ty cases = desugarLmlCaseWithFallback scrut (NE.head cases)

desugarNestedPatHelper :: LLmlPat LmlcTc -> MonadDesugar (Var, Var -> CoreExpr -> CoreExpr)
desugarNestedPatHelper (L _ innerPat) = case innerPat of
  LmlPatVar (FullName n, _) _ ->
    pure (Id $ Name n, \_ rhs -> rhs)
  LmlPatAny _ -> do
    freshV <- freshVar
    pure (freshV, \_ rhs -> rhs)
  LmlPatConstraint _ lPat _ ->
    desugarNestedPatHelper lPat
  LmlPatOr{} -> error "FIXME: Or patterns in tuples aren't supported."
  LmlPatConstruct _ (L _ constructorName) maybeLPat -> do
    case maybeLPat of
      Nothing -> do
        freshV <- freshVar
        pure (freshV, \_ rhs -> rhs)
      Just (L _ subPat) -> case subPat of
        LmlPatVar (FullName n, _) _ -> do
          freshV <- freshVar
          let fieldVar = Id $ Name n
          matchVar <- freshVar
          let wrapWithMatch _ rhs =
                let alt = (DataAlt (Name constructorName), [fieldVar], rhs)
                 in Match (Var freshV) matchVar (alt :| [])
          pure (freshV, wrapWithMatch)
        LmlPatAny _ -> do
          freshV <- freshVar
          wildcardVar <- freshVar
          matchVar <- freshVar
          let wrapWithMatch _ rhs =
                let alt = (DataAlt (Name constructorName), [wildcardVar], rhs)
                 in Match (Var freshV) matchVar (alt :| [])
          pure (freshV, wrapWithMatch)
        LmlPatTuple _ lPat lPats -> do
          freshV <- freshVar
          tupleVar <- freshVar
          matchVar1 <- freshVar

          let tuplePatterns = lPat : toList lPats
          tupleResults <- mapM desugarNestedPatHelper tuplePatterns
          let (tupleVars, tupleWrappers) = unzip tupleResults

          matchVar2 <- freshVar
          let wrapWithMatch _ rhs =
                let wrappedRhs = foldr (\(var, wrapper) acc -> wrapper var acc) rhs (zip tupleVars tupleWrappers)
                    tupleAlt = (TupleAlt, tupleVars, wrappedRhs)
                    tupleMatch = Match (Var tupleVar) matchVar2 (tupleAlt :| [])
                    constructorAlt = (DataAlt (Name constructorName), [tupleVar], tupleMatch)
                 in Match (Var freshV) matchVar1 (constructorAlt :| [])
          pure (freshV, wrapWithMatch)
        _ -> do
          freshV <- freshVar
          pure (freshV, \_ rhs -> rhs)
  _ -> do
    freshV <- freshVar
    pure (freshV, \_ rhs -> rhs)

desugarLLmlCase :: Var -> LLmlCase LmlcTc -> MonadDesugar CoreMatchAlt
desugarLLmlCase scrutineeVar (L _ case') = desugarLmlCase scrutineeVar case'

isSimpleVarOrWildcard :: LLmlPat LmlcTc -> Bool
isSimpleVarOrWildcard (L _ (LmlPatVar{})) = True
isSimpleVarOrWildcard (L _ (LmlPatAny _)) = True
isSimpleVarOrWildcard (L _ (LmlPatConstraint _ lPat _)) = isSimpleVarOrWildcard lPat
isSimpleVarOrWildcard _ = False

desugarLmlCase :: Var -> LmlCase LmlcTc -> MonadDesugar CoreMatchAlt
desugarLmlCase scrutineeVar (LmlCase ty (L _ pat) Nothing lExpr) = do
  expr <- desugarLLmlExpr lExpr
  case pat of
    LmlPatAny _ -> pure (DEFAULT, [], expr)
    LmlPatVar (FullName n, _) _ ->
      pure
        ( DEFAULT
        , []
        , replaceVar (Id $ Name n) scrutineeVar expr
        )
    LmlPatConstant _ lit -> pure (LitAlt $ desugarLmlLit lit, [], expr)
    LmlPatTuple _ lPat lPats -> do
      (vars, wrappers) <- mapAndUnzipM desugarNestedPatHelper (lPat : toList lPats)
      let exprWithMatches = foldr (\(var, wrapper) acc -> wrapper var acc) expr (zip vars wrappers)
      pure (TupleAlt, vars, exprWithMatches)
    LmlPatConstruct _ (L _ longident) maybeLPat ->
      let constuctorName = Name longident
       in case maybeLPat of
            Nothing -> pure (DataAlt constuctorName, [], expr)
            Just (L _ args) -> do
              case args of
                LmlPatVar (FullName n, _) _ -> do
                  pure (DataAlt constuctorName, [Id $ Name n], expr)
                LmlPatAny _ -> do
                  freshV <- freshVar
                  pure (DataAlt constuctorName, [freshV], expr)
                LmlPatTuple _ lPat lPats -> do
                  let pats = lPat : toList lPats
                      allSimple = all isSimpleVarOrWildcard pats
                  if allSimple
                    then do
                      (vars, wrappers) <- mapAndUnzipM desugarNestedPatHelper pats
                      let exprWithMatches = foldr (\(var, wrapper) acc -> wrapper var acc) expr (zip vars wrappers)
                      pure (DataAlt constuctorName, vars, exprWithMatches)
                    else do
                      vars <- replicateM (length pats) freshVar
                      pure (DataAlt constuctorName, vars, expr)
                LmlPatConstruct{} -> do
                  freshV <- freshVar
                  pure (DataAlt constuctorName, [freshV], expr)
                _ -> do
                  freshV <- freshVar
                  pure (DataAlt constuctorName, [freshV], expr)
    LmlPatOr{} -> error "FIXME: Or patterns in constructors aren't supported."
    LmlPatConstraint _ lPat _ -> desugarLmlCase scrutineeVar (LmlCase ty lPat Nothing lExpr)
desugarLmlCase _scrutineeVar (LmlCase _ty (L _ _pat) (Just _guardExpr) lExpr) = do
  expr <- desugarLLmlExpr lExpr
  pure (DEFAULT, [], expr)

desugarLmlCaseWithFallback :: Var -> LLmlCase LmlcTc -> CoreExpr -> MonadDesugar CoreMatchAlt
desugarLmlCaseWithFallback scrutineeVar (L _ (LmlCase ty (L _ pat) maybeGuard lExpr)) fallback = do
  rhsCore <- desugarLLmlExpr lExpr

  case pat of
    LmlPatAny _ -> do
      finalRhs <- case maybeGuard of
        Nothing -> pure rhsCore
        Just guardExpr -> do
          guardResult <- desugarLLmlExpr guardExpr
          guardVar <- freshVar
          let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
              falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
          pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
      pure (DEFAULT, [], finalRhs)
    LmlPatVar (FullName n, _) _ -> do
      let rhsWithBinding = replaceVar (Id $ Name n) scrutineeVar rhsCore
      finalRhs <- case maybeGuard of
        Nothing -> pure rhsWithBinding
        Just guardExpr -> do
          guardResult <- desugarLLmlExpr guardExpr
          let guardWithBinding = replaceVar (Id $ Name n) scrutineeVar guardResult
          guardVar <- freshVar
          let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsWithBinding)
              falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
          pure $ Match guardWithBinding guardVar (trueAlt :| [falseAlt])
      pure (DEFAULT, [], finalRhs)
    LmlPatConstant _ lit -> do
      finalRhs <- case maybeGuard of
        Nothing -> pure rhsCore
        Just guardExpr -> do
          guardResult <- desugarLLmlExpr guardExpr
          guardVar <- freshVar
          let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
              falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
          pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
      pure (LitAlt $ desugarLmlLit lit, [], finalRhs)
    LmlPatTuple _ lPat lPats -> do
      (vars, wrappers) <- mapAndUnzipM desugarNestedPatHelper (lPat : toList lPats)
      let pats = lPat : toList lPats
          hasNesting = not $ all isSimpleVarOrWildcard pats
      case (hasNesting, maybeGuard) of
        (True, Just guardExpr) -> do
          let applyWrappers rhs = foldr (\(var, wrapper) acc -> wrapper var acc) rhs (zip vars wrappers)
          guardCheck <- do
            guardResult <- desugarLLmlExpr guardExpr
            guardVar <- freshVar
            let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
            pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
          let finalRhs = applyWrappers guardCheck
          pure (TupleAlt, vars, finalRhs)
        _ -> do
          let applyWrappers rhs = foldr (\(var, wrapper) acc -> wrapper var acc) rhs (zip vars wrappers)
          finalRhs <- case maybeGuard of
            Nothing -> pure $ applyWrappers rhsCore
            Just guardExpr -> do
              guardResult <- desugarLLmlExpr guardExpr
              guardVar <- freshVar
              let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], applyWrappers rhsCore)
                  falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
              pure $ applyWrappers (Match guardResult guardVar (trueAlt :| [falseAlt]))
          pure (TupleAlt, vars, finalRhs)
    LmlPatConstraint _ lPat _ ->
      desugarLmlCaseWithFallback scrutineeVar (L (getLoc lPat) (LmlCase ty lPat maybeGuard lExpr)) fallback
    LmlPatConstruct _ (L _ longident) maybeLPat ->
      let constructorName = Name longident
       in case maybeLPat of
            Nothing -> do
              finalRhs <- case maybeGuard of
                Nothing -> pure rhsCore
                Just guardExpr -> do
                  guardResult <- desugarLLmlExpr guardExpr
                  guardVar <- freshVar
                  let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                      falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
                  pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
              pure (DataAlt constructorName, [], finalRhs)
            Just (L _ args) -> do
              case args of
                LmlPatVar (FullName n, _) _ -> do
                  finalRhs <- case maybeGuard of
                    Nothing -> pure rhsCore
                    Just guardExpr -> do
                      guardResult <- desugarLLmlExpr guardExpr
                      guardVar <- freshVar
                      let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                          falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
                      pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
                  pure (DataAlt constructorName, [Id $ Name n], finalRhs)
                LmlPatAny _ -> do
                  freshV <- freshVar
                  finalRhs <- case maybeGuard of
                    Nothing -> pure rhsCore
                    Just guardExpr -> do
                      guardResult <- desugarLLmlExpr guardExpr
                      guardVar <- freshVar
                      let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                          falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
                      pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
                  pure (DataAlt constructorName, [freshV], finalRhs)
                LmlPatTuple _ lPat lPats -> do
                  let pats = lPat : toList lPats
                      hasNesting = not $ all isSimpleVarOrWildcard pats
                  case (hasNesting, maybeGuard) of
                    (True, Just guardExpr) -> do
                      (vars, wrappers) <- mapAndUnzipM desugarNestedPatHelper pats
                      let applyWrappers rhs = foldr (\(var, wrapper) acc -> wrapper var acc) rhs (zip vars wrappers)
                      guardCheck <- do
                        guardResult <- desugarLLmlExpr guardExpr
                        guardVar <- freshVar
                        let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], rhsCore)
                            falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
                        pure $ Match guardResult guardVar (trueAlt :| [falseAlt])
                      let finalRhs = applyWrappers guardCheck
                      tupleVar <- freshVar
                      matchVar <- freshVar
                      let tupleAlt = (TupleAlt, vars, finalRhs)
                          tupleMatch = Match (Var tupleVar) matchVar (tupleAlt :| [])
                      pure (DataAlt constructorName, [tupleVar], tupleMatch)
                    _ -> do
                      (vars, wrappers) <- mapAndUnzipM desugarNestedPatHelper pats
                      let applyWrappers rhs = foldr (\(var, wrapper) acc -> wrapper var acc) rhs (zip vars wrappers)
                      finalRhs <- case maybeGuard of
                        Nothing ->
                          pure $ applyWrappers rhsCore
                        Just guardExpr -> do
                          guardResult <- desugarLLmlExpr guardExpr
                          guardVar <- freshVar
                          let guardCheckExpr =
                                let trueAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["true"]), [], applyWrappers rhsCore)
                                    falseAlt = (DataAlt (Name $ mkLongident $ stdPrefix :| ["false"]), [], fallback)
                                 in Match guardResult guardVar (trueAlt :| [falseAlt])
                          pure $ applyWrappers guardCheckExpr
                      tupleVar <- freshVar
                      matchVar <- freshVar
                      let tupleAlt = (TupleAlt, vars, finalRhs)
                          tupleMatch = Match (Var tupleVar) matchVar (tupleAlt :| [])
                      pure (DataAlt constructorName, [tupleVar], tupleMatch)
                _ -> do
                  let singleCase = L generatedSrcSpan (LmlCase ty (L generatedSrcSpan pat) maybeGuard lExpr)
                  alts <- compilePatternsWithBacktracking scrutineeVar (singleCase :| [])
                  pure $ NE.head alts
    _ -> do
      let singleCase = L generatedSrcSpan (LmlCase ty (L generatedSrcSpan pat) maybeGuard lExpr)
      alts <- compilePatternsWithBacktracking scrutineeVar (singleCase :| [])
      pure $ NE.head alts

desugarLLmlPat :: LLmlPat LmlcTc -> MonadDesugar Var
desugarLLmlPat (L _ pat) = desugarLmlPat pat

desugarLmlPat :: LmlPat LmlcTc -> MonadDesugar Var
desugarLmlPat = \case
  LmlPatVar (FullName n, _) _ -> pure $ Id $ Name n
  LmlPatConstraint _ lPat _ -> desugarLLmlPat lPat
  LmlPatAny _ -> freshVar
  _ -> error "FIXME: Only Var, Constraint and Any patterns are currently supported."

desugarLLmlBindGroup :: LLmlBindGroup LmlcTc -> MonadDesugar (NonEmpty CoreBind)
desugarLLmlBindGroup (L _ bindGroup) = desugarLmlBindGroup bindGroup

{- | Invariant of this function:
If we have 'Recursive' bind group then we have only one element in the list,
otherwise we have as many elements in the list as let exprs.
-}
desugarLmlBindGroup :: LmlBindGroup LmlcTc -> MonadDesugar (NonEmpty CoreBind)
desugarLmlBindGroup (LmlBindGroup _ NonRecursive lBinds) = do
  binds <- mapM desugarLLmlBind lBinds
  pure $ fmap (uncurry NonRec) binds
desugarLmlBindGroup (LmlBindGroup _ Recursive lBinds) = do
  binds <- mapM desugarLLmlBind lBinds
  pure $ pure $ Rec binds

desugarLLmlBind :: LLmlBind LmlcTc -> MonadDesugar (Var, CoreExpr)
desugarLLmlBind (L _ bind) = desugarLmlBind bind

desugarLmlBind :: LmlBind LmlcTc -> MonadDesugar (Var, CoreExpr)
desugarLmlBind (LmlBind _ lPat lExpr) = liftA2 (,) (desugarLLmlPat lPat) (desugarLLmlExpr lExpr)

replaceVar :: Var -> Var -> CoreExpr -> CoreExpr
replaceVar oldVar newVar = \case
  var@(Var id') -> if id' == oldVar then Var newVar else var
  lit@(Lit _) -> lit
  App leftExpr rightExpr -> App (replaceVar oldVar newVar leftExpr) (replaceVar oldVar newVar rightExpr)
  lam@(Lam var expr) -> if var == oldVar then lam else Lam var (replaceVar oldVar newVar expr)
  Let bind expr ->
    let (newBind, control) = replaceVarBind oldVar newVar bind
     in if control then Let newBind (replaceVar oldVar newVar expr) else Let newBind expr
  Match scrutinee scrutineeVar alts ->
    if scrutineeVar == oldVar
      then Match (replaceVar oldVar newVar scrutinee) scrutineeVar alts
      else Match (replaceVar oldVar newVar scrutinee) scrutineeVar (fmap (replaceVarMatchAlt oldVar newVar) alts)
  Tuple expr exprs -> Tuple (replaceVar oldVar newVar expr) $ fmap (replaceVar oldVar newVar) exprs

{- | Replaces variable in 'Bind'.
If this binding binds variable then we emit 'False' to stop replacing further, otherwise return 'True'.
-}
replaceVarBind :: Var -> Var -> CoreBind -> (CoreBind, Bool)
replaceVarBind oldVar newVar = \case
  nr@(NonRec var expr) -> if var == oldVar then (nr, False) else (NonRec newVar (replaceVar oldVar newVar expr), True)
  r@(Rec binds) ->
    if elem oldVar $ fmap fst binds
      then (r, False)
      else (Rec $ fmap (second (replaceVar oldVar newVar)) binds, True)

replaceVarMatchAlt :: Var -> Var -> CoreMatchAlt -> CoreMatchAlt
replaceVarMatchAlt oldVar newVar alt@(altCon, boundVars, expr) =
  if oldVar `elem` boundVars then alt else (altCon, boundVars, replaceVar oldVar newVar expr)

desugarLLmlDecl :: Maybe (LLongident LmlcTc) -> LLmlDecl LmlcTc -> MonadDesugar [CoreBind]
desugarLLmlDecl moduleName (L _ decl) = desugarLmlDecl moduleName decl

desugarLmlDecl :: Maybe (LLongident LmlcTc) -> LmlDecl LmlcTc -> MonadDesugar [CoreBind]
desugarLmlDecl moduleName = \case
  ValD _ lBindGroup -> toList <$> desugarLLmlBindGroup lBindGroup
  TyD _ tyDecls -> concat <$> mapM (desugarLTyDecl moduleName) (toList tyDecls)
  _ -> pure []

desugarLTyDecl :: Maybe (LLongident LmlcTc) -> LTyDecl LmlcTc -> MonadDesugar [CoreBind]
desugarLTyDecl moduleName (L _ (DataDecl _ (L _ _typeName) _params conDecls)) =
  mapM (desugarLConDeclToBind moduleName) conDecls
desugarLTyDecl _ _ = pure []

desugarLConDeclToBind :: Maybe (LLongident LmlcTc) -> LConDecl LmlcTc -> MonadDesugar CoreBind
desugarLConDeclToBind moduleName (L _ (ConDecl _ (L _ conName) _args)) = do
  let qualifiedName = case moduleName of
        Just (L _ (Longident modNameNE)) -> Longident $ modNameNE <> pure conName
        Nothing -> mkLongident $ pure conName
      conVar = Id $ Name qualifiedName
  pure $ NonRec conVar (Var conVar)

desugarLmlModule :: LmlModule LmlcTc -> MonadDesugar [CoreBind]
desugarLmlModule (LmlModule _ moduleName lDecls) = concatMapM (desugarLLmlDecl moduleName) lDecls
