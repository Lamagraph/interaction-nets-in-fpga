module Lamagraph.Compiler.Core.LmlToCore (desugarLmlModule) where

import Relude

import Control.Monad.Extra
import Data.Foldable.Extra hiding (elem)

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.MrTypes
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
    cases <- mapM (desugarLLmlCase scrutineeVar) lCases
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

desugarLLmlCase :: Var -> LLmlCase LmlcTc -> MonadDesugar CoreMatchAlt
desugarLLmlCase scrutineeVar (L _ case') = desugarLmlCase scrutineeVar case'

desugarLmlCase :: Var -> LmlCase LmlcTc -> MonadDesugar CoreMatchAlt
desugarLmlCase scrutineeVar (LmlCase _ (L _ pat) Nothing lExpr) = do
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
    LmlPatTuple _ lPat lPats ->
      let vars = map helper (lPat : toList lPats)
       in pure (TupleAlt, vars, expr)
    LmlPatConstruct _ (L _ longident) maybeLPat ->
      let constuctorName = Name longident
       in case maybeLPat of
            Nothing -> pure (DataAlt constuctorName, [], expr)
            Just (L _ args) ->
              case args of
                LmlPatVar (FullName n, _) _ -> pure (DataAlt constuctorName, [Id $ Name n], expr)
                LmlPatTuple _ lPat lPats ->
                  let vars = map helper (lPat : toList lPats)
                   in pure (DataAlt constuctorName, vars, expr)
                _ -> error "Internal error: Constructors can only be applied to Var or Tuple."
    LmlPatOr{} -> error "FIXME: Or patterns in match expressions aren't supported."
    LmlPatConstraint{} -> error "FIXME: Constraints in pattern-matching are currently unsupported."
 where
  helper lPat = case unLoc lPat of
    LmlPatVar (FullName n, _) _ -> Id $ Name n
    _ -> error "FIXME: Nested patterns are currently unsupported."
desugarLmlCase _ (LmlCase _ _ (Just _) _) = error "FIXME: Guards in pattern-matching are currently unsupported."

desugarLLmlPat :: LLmlPat LmlcTc -> MonadDesugar Var
desugarLLmlPat (L _ pat) = desugarLmlPat pat

desugarLmlPat :: LmlPat LmlcTc -> MonadDesugar Var
desugarLmlPat = \case
  LmlPatVar (FullName n, _) _ -> pure $ Id $ Name n
  LmlPatConstraint _ lPat _ -> desugarLLmlPat lPat
  _ -> error "FIXME: Only Var and Constraint patterns are currently supported."

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

desugarLLmlDecl :: LLmlDecl LmlcTc -> MonadDesugar [CoreBind]
desugarLLmlDecl (L _ decl) = desugarLmlDecl decl

desugarLmlDecl :: LmlDecl LmlcTc -> MonadDesugar [CoreBind]
desugarLmlDecl = \case
  ValD _ lBindGroup -> toList <$> desugarLLmlBindGroup lBindGroup
  _ -> pure []

desugarLmlModule :: LmlModule LmlcTc -> MonadDesugar [CoreBind]
desugarLmlModule (LmlModule _ _ lDecls) = concatMapM desugarLLmlDecl lDecls
