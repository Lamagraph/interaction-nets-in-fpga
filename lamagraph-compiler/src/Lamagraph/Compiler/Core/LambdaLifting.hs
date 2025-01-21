{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}
{-# HLINT ignore "Eta reduce" #-}
module Lamagraph.Compiler.Core.LambdaLifting (freeVars, lambdaLiftingProgram) where

import Control.Monad.Extra
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.MonadDesugar (MonadDesugar, freshVar)
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.TcTypes
import Relude

without :: (Foldable t, Eq a) => [a] -> t a -> [a]
left `without` right = foldr (filter . (/=)) left right

manyAbstraction :: CoreExpr -> [Var] -> CoreExpr
manyAbstraction e vars = List.foldl (flip Lam) e vars

applyTo :: CoreExpr -> [Var] -> CoreExpr
applyTo = List.foldl (\e a -> App e $ Var a)

getVarsFromBind :: CoreBind -> [Var]
getVarsFromBind = \case
  NonRec v _ -> [v]
  Rec binds -> NonEmpty.toList $ NonEmpty.map fst binds

-- | Get free vars from expression
freeVars :: CoreExpr -> [Var]
freeVars (Var v) = [v]
freeVars (App lExpr rExpr) = List.nub $ freeVars lExpr ++ freeVars rExpr
freeVars (Lam var expr) = List.nub $ filter (/= var) $ freeVars expr
freeVars (Let (NonRec var varBody) bodyExpr) =
  List.nub $
    filter (/= var) $
      freeVars bodyExpr ++ freeVars varBody
freeVars (Let (Rec binds) bodyExpr) =
  List.nub $
    concatMap freeVars (bodyExpr NonEmpty.<| varsBodyExpr) `without` vars
 where
  (vars, varsBodyExpr) = NonEmpty.unzip binds
freeVars (Match casedExpression v alts) =
  List.nub $
    freeVars casedExpression ++ concatMap altFreeVars alts
 where
  altFreeVars :: CoreMatchAlt -> [Var]
  altFreeVars (_, bindVars, expr) = freeVars expr `without` (v : bindVars)
freeVars (Tuple firstExpression otherExpressions) =
  toList $
    List.nub $
      concatMap freeVars $
        firstExpression NonEmpty.<| otherExpressions
freeVars _ = []

freeVarsBind :: CoreBind -> NonEmpty [Var]
freeVarsBind = \case
  NonRec v expr -> pure $ pureBind [v] expr
  Rec binds ->
    let recVars = NonEmpty.map fst binds
     in NonEmpty.map (\(_, e) -> pureBind recVars e) binds
 where
  pureBind vars e = freeVars e `without` vars

allVars :: CoreExpr -> [Var]
allVars = \case
  Var v -> [v]
  App l r -> allVars l ++ allVars r
  Lam v e -> v : allVars e
  Let (NonRec v varBody) body -> v : allVars varBody ++ allVars body
  Let (Rec binds) body -> concatMap (\(v, vB) -> v : allVars vB) binds ++ allVars body
  Tuple fstE otherE -> allVars fstE ++ concatMap allVars otherE
  Match casedE v alts -> v : allVars casedE ++ concatMap (\(_, vars, altE) -> vars ++ allVars altE) alts
  Lit _ -> []

{- | substitute expression instead of var in some other expression

__NOTE__: it can not substitute if var is bounded
-}
substExprByVar :: CoreExpr -> Var -> CoreExpr -> CoreExpr
substExprByVar expr var =
  \case
    Var v -> if v == var then expr else Var v
    l@(Lit _) -> l
    App le re -> App (sub le) (sub re)
    Lam v e ->
      if v == var
        then error "an attempt to change bounded var (var == lambded var)"
        else
          if var `isFreeIn` e
            then
              Lam v $ sub e
            else
              if var `notElem` allVars e
                then Lam v e
                else error "an attempt to change bounded var (var is not free in e)"
    Let (NonRec v varE) bodyE ->
      if v == var
        then error "an attempt to change letted var"
        else Let (NonRec v $ sub varE) $ sub bodyE
    Let (Rec binds) bodyE ->
      if var `elem` NonEmpty.map fst binds
        then error "an attempt to change letted (rec) var"
        else Let (Rec (NonEmpty.map (second sub) binds)) $ sub bodyE
    Match casedExpr v alts ->
      if var == v
        then error "an attempt to change bounded by match synonym var"
        else
          if var `elem` concatMap (\(_, vars, _) -> vars) alts
            then error "an attempt to change matched var"
            else Match (sub casedExpr) v $ NonEmpty.map (\(con, vars, e) -> (con, vars, sub e)) alts
    Tuple fstE otherE -> Tuple (sub fstE) $ NonEmpty.map sub otherE
 where
  sub = substExprByVar expr var
  v `isFreeIn` e = v `elem` freeVars e

defaultGlobals :: [Var]
defaultGlobals = map Id (HashMap.keys hashMap)
 where
  TyEnv hashMap = defaultEnv

-- | Lambda lifting. It returns list of top level bindings and lifted expression
liftLams :: [Var] -> CoreExpr -> MonadDesugar ([CoreBind], CoreExpr)
liftLams globals = \case
  App le re -> do
    (lBinds, lLifted) <- liftLams globals le
    (rBinds, rLifted) <- liftLams globals re
    return
      ( lBinds ++ rBinds
      , App
          lLifted
          rLifted
      )
  Lam var expr -> do
    (binds, lifted, lamVars) <- accumulateLams expr
    fresh <- freshVar
    let bindedVars = concatMap getVarsFromBind binds
        fVars = freeVars expr `without` (var : globals ++ bindedVars ++ lamVars)
        newBind = NonRec fresh $ manyAbstraction lifted (var : lamVars ++ fVars)
    return (binds ++ [newBind], Var fresh `applyTo` reverse fVars)
  Let (NonRec var varBody) inBody -> do
    (varBinds, varLifted, lamVars) <- accumulateLams varBody
    let bindedVars = concatMap getVarsFromBind varBinds
        freeVarsVarBody = freeVars varBody `without` (var : globals ++ bindedVars ++ lamVars)
    (bodyBinds, bodyLifted) <- liftLams globals inBody
    if var `elem` globals
      then
        let liftedExpression = substExprByVar (Var var `applyTo` reverse freeVarsVarBody) var bodyLifted
            newBind = NonRec var $ manyAbstraction varLifted (lamVars ++ freeVarsVarBody)
         in return
              ( varBinds
                  ++ bodyBinds
                  ++ [newBind]
              , liftedExpression
              )
      else do
        fresh <- freshVar
        let liftedExpression = substExprByVar (Var fresh `applyTo` reverse freeVarsVarBody) var bodyLifted
            newBind = NonRec fresh $ manyAbstraction varLifted (lamVars ++ freeVarsVarBody)
         in return
              ( varBinds
                  ++ bodyBinds
                  ++ [newBind]
              , liftedExpression
              )
  Let b@(Rec binds) body -> do
    let bindsList = NonEmpty.toList binds
    (varBinds, varsLifted, varsForLams) <-
      unzip3
        <$> mapM
          (\(_, varBody) -> accumulateLams varBody)
          bindsList
    let freeVarsVarBody =
          map
            (`without` (globals ++ concatMap getVarsFromBind (concat varBinds) ++ concat varsForLams))
            (NonEmpty.toList $ freeVarsBind b)
    (bodyBinds, bodyLifted) <- liftLams globals body
    newVarBinds <-
      mapM
        ( \(lifted, lamVars, fVars, (var, _)) ->
            if var `notElem` globals
              then do
                fresh <- freshVar
                return (fresh, manyAbstraction lifted (lamVars ++ fVars))
              else return (var, manyAbstraction lifted (lamVars ++ fVars))
        )
        (List.zip4 varsLifted varsForLams freeVarsVarBody bindsList)
    let substitutedBody =
          List.foldl
            (\bodyLiftedSubst ((v, _), (fresh, _), fVars) -> substExprByVar (Var fresh `applyTo` reverse fVars) v bodyLiftedSubst)
            bodyLifted
            (zip3 bindsList newVarBinds freeVarsVarBody)
    return (concat varBinds ++ bodyBinds ++ [Rec $ NonEmpty.fromList (bindsList `substFresh` newVarBinds)], substitutedBody)
   where
    substFresh :: [(Var, CoreExpr)] -> [(Var, CoreExpr)] -> [(Var, CoreExpr)]
    oldBinds `substFresh` newBinds =
      List.foldl
        (\bind ((old, _), (new, _)) -> map (second (substExprByVar (Var new) old)) bind)
        newBinds
        (zip oldBinds newBinds)
  Match casedExpr v alts -> do
    (casedBind, casedLifted) <- liftLams globals casedExpr
    (altBinds, altsBodyLifted) <- mapAndUnzipM (\(_, _, altExpr) -> liftLams globals altExpr) (NonEmpty.toList alts)
    let altsLifted =
          NonEmpty.zipWith
            (\(con, vars, _) lifted -> (con, vars, lifted))
            alts
            (NonEmpty.fromList altsBodyLifted)
        matchExpr = Match casedLifted v altsLifted
    return (concat altBinds ++ casedBind, matchExpr)
  Tuple fstExpr otherExprs -> do
    (fstBinds, fstLifted) <- liftLams globals fstExpr
    (otherBinds, othersLifted) <- mapAndUnzipM (liftLams globals) (NonEmpty.toList otherExprs)
    return (fstBinds ++ concat otherBinds, Tuple fstLifted (NonEmpty.fromList othersLifted))
  e -> return ([], e)
 where
  accumulateLams = \case
    Lam v e -> do
      (binds, lifted, vars) <- accumulateLams e
      return (binds, lifted, vars ++ [v])
    e -> do
      (binds, lifted) <- liftLams globals e
      return (binds, lifted, [])

-- | Evaluate lambda lifting for Bind.
lambdaLiftingBind :: [Var] -> CoreBind -> MonadDesugar [CoreBind]
lambdaLiftingBind globals b = do
  (binds, _) <- liftLams globals $ Let b (Lit $ LitInt 0) -- kinda hack
  return binds

lambdaLiftingProgram :: [CoreBind] -> MonadDesugar [CoreBind]
lambdaLiftingProgram bs = concatMapM (lambdaLiftingBind globals) bs
 where
  globals = defaultGlobals ++ concatMap getTopLevelVars bs
  getTopLevelVars = \case
    NonRec v _ -> [v]
    Rec binds -> NonEmpty.toList $ NonEmpty.map fst binds
