{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Lamagraph.Compiler.Core.Anf (
  bindsLlAnf,
  ImmExpr (..),
  CompExpr (..),
  AExpr (..),
  ABind (..),
  AMatchAlt,
) where

import Data.Foldable

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.LambdaLifting
import Lamagraph.Compiler.Core.MonadDesugar
import Relude

data ImmExpr
  = ImmVar Var
  | ImmLit Literal
  | ImmTuple ImmExpr (NonEmpty ImmExpr)
  | ImmLam Var AExpr

data CompExpr
  = CompApp ImmExpr (NonEmpty ImmExpr)
  | CompMatch ImmExpr Var (NonEmpty AMatchAlt)
  | CompImmExpr ImmExpr

data AExpr
  = ALet ABind AExpr
  | ACompExpr CompExpr

data ABind = ANonRec Var CompExpr | ARec (NonEmpty (Var, CompExpr))

type AMatchAlt = (AltCon, [Var], AExpr)

bindsLlAnf :: [CoreBind] -> MonadDesugar [ABind]
bindsLlAnf bind = lambdaLiftingProgram bind >>= bindsToAnf
 where
  bindsToAnf :: [CoreBind] -> MonadDesugar [ABind]
  bindsToAnf binds = mapM bindToAnf binds
  bindToAnf :: CoreBind -> MonadDesugar ABind
  bindToAnf (NonRec v e) = do
    anf <- anfSimplify <$> toAnf e exprWithHoleBasic
    case anf of
      ACompExpr compExpr -> pure $ ANonRec v compExpr
      _ -> error "anf error"
  bindToAnf _ = undefined

-- bindToAnf (Rec binds) = Rec <$> desugarAnf
--  where
--   desugarAnf = do
--     anfBinds <-
--       mapM
--         ( \(v, e) -> do
--             anfE <- toAnf e exprWithHoleBasic
--             return (v, anfToCore $ anfSimplify anfE)
--         )
--         (NonEmpty.toList binds)
--     return $ NonEmpty.fromList anfBinds

-- arityAnalysis :: HashMap Var Int -> CoreBind -> HashMap Var Int
-- arityAnalysis hashMap = \case
--   NonRec v e -> HashMap.insert v (accumulateLams 0 hashMap e) hashMap
--   Rec binds -> List.foldl (\hM (v, e) -> HashMap.insert v (accumulateLams 0 hM e) hM) hashMap binds
--  where
--   accumulateLams acc hM = \case
--     Var v -> case HashMap.lookup v hM of
--       Nothing -> acc
--       Just ar -> acc + ar
--     Lit _ -> acc
--     Tuple _ _ -> acc
--     Match _ _ ((_, _, fstAlt) NonEmpty.:| _) -> acc + accumulateLams 0 hM fstAlt -- it assumed that every case branch are equivalent in terms of arity
--     App le re -> acc + (accumulateLams 0 hM le - accumulateLams 0 hM re) -- it will be nonsense if re has arity > 1
--     Lam _ e -> accumulateLams (acc + 1) hM e
--     Let _ _ -> error "after LL there is should not be nested let in expression"

toAnf ::
  CoreExpr ->
  (ImmExpr -> MonadDesugar AExpr) ->
  MonadDesugar AExpr
-- trivial cases
toAnf (Var var) exprWithHole = exprWithHole (ImmVar var)
toAnf (Lit l) exprWithHole = exprWithHole (ImmLit l)
-- lambda abstraction
toAnf (Lam var body) exprWithHole = do
  (vars, aBody) <- accumulateLambdas body
  exprWithHole $ ImmLam var (manyAbs aBody vars)
 where
  accumulateLambdas = \case
    Lam v b -> do
      (vars, aB) <- accumulateLambdas b
      return (vars ++ [v], aB)
    e -> do
      aE <- toAnf e exprWithHole
      return ([], aE)
  manyAbs e vs = List.foldl (\lE v -> ACompExpr $ CompImmExpr $ ImmLam v lE) e vs
-- application of expressions
toAnf a@(App l r) exprWithHole =
  case accumulateApplies a of
    Var v : args -> do
      freshVars <-
        mapM
          (const freshVar)
          args
      resultVar <- freshVar
      resultOfApps <- exprWithHole $ ImmVar resultVar
      let applies = ALet (ANonRec resultVar (CompApp (ImmVar v) (NonEmpty.fromList $ map ImmVar freshVars))) resultOfApps
      foldlM
        ( \aExpr (fresh, arg) -> do
            toAnf arg (\immArg -> return $ ALet (ANonRec fresh (CompImmExpr immArg)) aExpr)
        )
        applies
        (zip freshVars args)
    _ -> basicCase l r
 where
  accumulateApplies :: CoreExpr -> [CoreExpr]
  accumulateApplies (App le re) = accumulateApplies le ++ [re]
  accumulateApplies e = List.singleton e
  basicCase lExpr rExpr =
    toAnf
      lExpr
      ( \immLeft -> do
          toAnf
            rExpr
            ( \immRight -> do
                fresh <- freshVar
                ALet (ANonRec fresh (CompApp immLeft (NonEmpty.singleton immRight))) <$> exprWithHole (ImmVar fresh)
                -- undefined
            )
      )
-- tuple
toAnf (Tuple fstExpr otherExprs) exprWithHole = do
  fstFreshVar <- freshVar
  otherFreshVars <- mapM (const freshVar) otherExprs
  tupleSimple <- exprWithHole $ ImmTuple (ImmVar fstFreshVar) (NonEmpty.map ImmVar otherFreshVars)
  fstAExpr <-
    toAnf
      fstExpr
      ( \immFstExpr -> return $ ALet (ANonRec fstFreshVar (CompImmExpr immFstExpr)) tupleSimple
      )
  foldlM
    ( \aExpr (coreExp, resVar) -> do
        toAnf
          coreExp
          ( \immElementExpr -> do
              return $ ALet (ANonRec resVar (CompImmExpr immElementExpr)) aExpr
          )
    )
    fstAExpr
    (NonEmpty.zip otherExprs otherFreshVars)

-- nonrecursive let binding
toAnf (Let (NonRec var varBody) body) exprWithHole =
  toAnf
    varBody
    ( \immVarBody -> do
        anfBody <- toAnf body exprWithHole
        return $ ALet (ANonRec var (CompImmExpr immVarBody)) anfBody
    )
-- recursive let binding
toAnf (Let (Rec binds) body) exprWithHole = do
  anfBody <- toAnf body exprWithHole
  recBinds <-
    mapM
      ( \(var, e) -> do
          aE <- varBodyMatching e
          return (var, aE)
      )
      binds
  return $ ALet (ARec recBinds) anfBody
 where
  varBodyMatching varBody = do
    a <- toAnf varBody exprWithHole
    let e = case a of
          ACompExpr compE -> compE -- it works only after lambda lifting
          _ -> error "an attempt to use anf conversation without CC and LL"
    return e

-- pattern matching
toAnf (Match casedExpr v alts) exprWithHole = do
  -- var for cased expression
  resCasedVar <- freshVar
  toAnf
    casedExpr
    ( \immExpr ->
        do
          -- transform to anf every case separately
          aAlts <- mapM coreAltToAAlt alts
          return $
            ALet
              ( ANonRec
                  resCasedVar
                  (CompImmExpr immExpr)
              )
              ( ACompExpr $
                  CompMatch
                    (ImmVar resCasedVar)
                    v
                    aAlts
              )
    )
 where
  coreAltToAAlt :: CoreMatchAlt -> MonadDesugar AMatchAlt
  coreAltToAAlt (con, vars, altExpr) = do
    aAltExpr <- toAnf altExpr exprWithHole
    return (con, vars, aAltExpr)

-- toAnf _ _ = undefined
exprWithHoleBasic :: (Monad m) => ImmExpr -> m AExpr
exprWithHoleBasic x = return $ ACompExpr $ CompImmExpr x

anfToCore :: AExpr -> CoreExpr
anfToCore (ALet (ANonRec v varBody) body) = Let (NonRec v (cAstToCore varBody)) (anfToCore body)
anfToCore (ALet (ARec binds) body) = Let (Rec coreBinds) (anfToCore body)
 where
  coreBinds = NonEmpty.map (second cAstToCore) binds
anfToCore (ACompExpr e) = cAstToCore e

cAstToCore :: CompExpr -> CoreExpr
cAstToCore (CompApp lExp rExps) = iAstToCore lExp `applyTo` NonEmpty.map iAstToCore rExps
 where
  expr `applyTo` exprs = List.foldl App expr (NonEmpty.toList exprs)
cAstToCore (CompImmExpr e) = iAstToCore e
cAstToCore (CompMatch casedExpr v alts) = Match (iAstToCore casedExpr) v coreAlts
 where
  coreAlts = NonEmpty.map (\(con, vars, matchBody) -> (con, vars, anfToCore matchBody)) alts

iAstToCore :: ImmExpr -> CoreExpr
iAstToCore (ImmVar v) = Var v
iAstToCore (ImmLit l) = Lit l
iAstToCore (ImmTuple fstExpr otherExps) = Tuple (iAstToCore fstExpr) $ NonEmpty.map iAstToCore otherExps
iAstToCore (ImmLam v body) = Lam v $ anfToCore body

anfSimplify :: AExpr -> AExpr
-- let v = cExpr in v  ~~> cExpr
anfSimplify e@(ALet (ANonRec v varBody) (ACompExpr (CompImmExpr (ImmVar v')))) = if v == v' then ACompExpr varBody else e
-- let v = ImmExpr in body ~~> subst ImmExpr instead of v in body
anfSimplify (ALet (ANonRec v (CompImmExpr immExpr)) exprNext) = anfSimplify $ aSubstituteVar v immExpr exprNext
-- just a steps
anfSimplify (ALet (ANonRec v vBody) exprNext) = ALet (ANonRec v vBody) $ anfSimplify exprNext
anfSimplify (ALet (ARec binds) exprNext) = ALet (ARec (NonEmpty.map bindSimplify binds)) $ anfSimplify exprNext
 where
  bindSimplify (v', CompImmExpr (ImmLam var aExpr)) = (v', CompImmExpr $ ImmLam var $ anfSimplify aExpr)
  bindSimplify b = b
anfSimplify (ACompExpr (CompMatch immCasedExpr v alts)) = ACompExpr $ CompMatch immCasedExpr v $ NonEmpty.map (\(con, vars, e) -> (con, vars, anfSimplify e)) alts
anfSimplify (ACompExpr (CompImmExpr (ImmLam v e))) = ACompExpr (CompImmExpr (ImmLam v $ anfSimplify e))
anfSimplify e = e

iSubstituteVar :: Var -> ImmExpr -> ImmExpr -> ImmExpr
iSubstituteVar substitutedVar resOfSubst expr = case expr of
  e@(ImmVar v) -> if v == substitutedVar then resOfSubst else e
  (ImmTuple fstImmExpr otherImmExprs) -> ImmTuple (subI fstImmExpr) (NonEmpty.map subI otherImmExprs)
  ImmLam v aExpr -> ImmLam v $ subA aExpr
  e -> e
 where
  subI = iSubstituteVar substitutedVar resOfSubst
  subA = aSubstituteVar substitutedVar resOfSubst

cSubstituteVar :: Var -> ImmExpr -> CompExpr -> CompExpr
cSubstituteVar substitutedVar resOfSubst expr = case expr of
  CompApp leftImmExpr rightImmExpr -> CompApp (subI leftImmExpr) (NonEmpty.map subI rightImmExpr)
  CompImmExpr immExpr -> CompImmExpr $ subI immExpr
  CompMatch immCasedExpr v alts -> CompMatch (subI immCasedExpr) v (NonEmpty.map (\(con, vars, aExpr) -> (con, vars, subA aExpr)) alts)
 where
  subI = iSubstituteVar substitutedVar resOfSubst
  subA = aSubstituteVar substitutedVar resOfSubst

aSubstituteVar :: Var -> ImmExpr -> AExpr -> AExpr
aSubstituteVar substitutedVar resOfSubst expr = case expr of
  e@(ALet (ANonRec v compExpr) body) -> case resOfSubst of
    ImmVar substituteVar -> if v == substituteVar then e else ALet (ANonRec v $ subC compExpr) $ subA body
    _ -> ALet (ANonRec v $ subC compExpr) $ subA body
  (ALet (ARec binds) body) -> ALet (ARec $ NonEmpty.map (second subC) binds) $ subA body
  ACompExpr compExpr -> ACompExpr $ subC compExpr
 where
  subC = cSubstituteVar substitutedVar resOfSubst
  subA = aSubstituteVar substitutedVar resOfSubst
