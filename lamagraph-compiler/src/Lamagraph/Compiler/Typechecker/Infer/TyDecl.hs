module Lamagraph.Compiler.Typechecker.Infer.TyDecl (
  inferTyDecls,
) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper (freshTVar, unzip3F)
import Lamagraph.Compiler.Typechecker.Infer.Type
import Lamagraph.Compiler.Typechecker.TcTypes (
  MonadTypecheck,
  Name (..),
  Ty (..),
  TyConstrEnv (..),
  TyConstrInfo (..),
  TyEnv (..),
  TyScheme (..),
  TypecheckError (..),
 )

inferTyDecls ::
  Maybe (LLongident LmlcMr) ->
  TyEnv ->
  TyConstrEnv ->
  NonEmpty (LTyDecl LmlcMr) ->
  MonadTypecheck (TyEnv, TyConstrEnv, NonEmpty (LTyDecl LmlcTc))
inferTyDecls moduleName tyEnv tyConstrEnv tyDecls = do
  let newTyConstrs = mapMaybe (extractTyConstrInfo moduleName) (toList tyDecls)
      TyConstrEnv oldTyConstrMap = tyConstrEnv
      extendedTyConstrEnv = TyConstrEnv $ HashMap.union (HashMap.fromList newTyConstrs) oldTyConstrMap

  results <- traverse (inferLTyDecl moduleName tyEnv extendedTyConstrEnv) tyDecls
  let (envs, tyConstrEnvs, tyDeclsTyped) = unzip3F results
      combinedEnv = TyEnv $ HashMap.unions (toList $ fmap (\(TyEnv m) -> m) envs)
      TyConstrEnv finalTyConstrMap = tyConstrEnv
      updatedTyConstrEnvs = fmap (\(TyConstrEnv m) -> m) tyConstrEnvs
      mergedTyConstrMap = foldr HashMap.union finalTyConstrMap updatedTyConstrEnvs
  pure (combinedEnv, TyConstrEnv mergedTyConstrMap, tyDeclsTyped)

qualifyTypeName :: Maybe (LLongident LmlcMr) -> Text -> Name
qualifyTypeName (Just (L _ (Longident modNameNE))) typeName =
  Name $ Longident $ modNameNE <> pure typeName
qualifyTypeName Nothing typeName =
  Name $ mkLongident $ pure typeName

extractTyConstrInfo :: Maybe (LLongident LmlcMr) -> LTyDecl LmlcMr -> Maybe (Name, TyConstrInfo)
extractTyConstrInfo _moduleName (L _ (AliasDecl{})) =
  Nothing
extractTyConstrInfo moduleName (L _ (DataDecl _ (L _ name) params _)) =
  let qualifiedName = qualifyTypeName moduleName name
   in Just (qualifiedName, DataConstr (length params))

extractTypeParamName :: LLmlType LmlcMr -> MonadTypecheck Name
extractTypeParamName (L _ (LmlTyVar _ (L _ name))) = pure $ Name $ mkLongident $ pure name
extractTypeParamName _ = throwError InvalidTypeParameter

checkDuplicates :: (Hashable a) => (a -> TypecheckError) -> [a] -> MonadTypecheck ()
checkDuplicates onError items =
  void $
    foldlM
      ( \acc item ->
          if HashMap.member item acc
            then throwError (onError item)
            else pure $ HashMap.insert item () acc
      )
      HashMap.empty
      items

inferLTyDecl ::
  Maybe (LLongident LmlcMr) ->
  TyEnv ->
  TyConstrEnv ->
  LTyDecl LmlcMr ->
  MonadTypecheck (TyEnv, TyConstrEnv, LTyDecl LmlcTc)
inferLTyDecl moduleName tyEnv tyConstrEnv (L loc tyDecl) = over _3 (L loc) <$> inferTyDecl moduleName tyEnv tyConstrEnv tyDecl

inferTyDecl ::
  Maybe (LLongident LmlcMr) -> TyEnv -> TyConstrEnv -> TyDecl LmlcMr -> MonadTypecheck (TyEnv, TyConstrEnv, TyDecl LmlcTc)
inferTyDecl moduleName _tyEnv tyConstrEnv (AliasDecl _ name@(L _ aliasName) params rhs) = do
  typeParamNames <- traverse extractTypeParamName params
  freshParamTys <- replicateM (length typeParamNames) freshTVar
  let freshNames = [n | TVar n <- freshParamTys]
      typeParamEnv = HashMap.fromList $ zip typeParamNames freshParamTys

  (rhsTy, rhsTyped) <- lLmlTypeToTyStrict typeParamEnv tyConstrEnv rhs
  paramsTyped <- traverse (fmap snd . lLmlTypeToTy tyConstrEnv) params

  let aliasInfo = TypeAlias freshNames rhsTy
      qualifiedAliasName = qualifyTypeName moduleName aliasName
      TyConstrEnv oldMap = tyConstrEnv
      updatedMap = HashMap.insert qualifiedAliasName aliasInfo oldMap
      updatedTyConstrEnv = TyConstrEnv updatedMap

  pure (TyEnv HashMap.empty, updatedTyConstrEnv, AliasDecl noExtField name paramsTyped rhsTyped)
inferTyDecl moduleName _tyEnv tyConstrEnv (DataDecl _ name params constructors) = do
  paramsTyped <- traverse (fmap snd . lLmlTypeToTy tyConstrEnv) params
  (constrsEnv, constructorsTyped) <- inferDataConstructors moduleName name params tyConstrEnv constructors
  pure (constrsEnv, tyConstrEnv, DataDecl noExtField name paramsTyped constructorsTyped)

inferDataConstructors ::
  Maybe (LLongident LmlcMr) ->
  XLocated LmlcMr Text ->
  [LLmlType LmlcMr] ->
  TyConstrEnv ->
  [LConDecl LmlcMr] ->
  MonadTypecheck (TyEnv, [LConDecl LmlcTc])
inferDataConstructors moduleName (L _ typeName) typeParams tyConstrEnv constructors = do
  typeParamNames <- traverse extractTypeParamName typeParams
  checkDuplicates DuplicateTypeParameter typeParamNames

  let constrNames = fmap (\(L _ (ConDecl _ (L _ name) _)) -> Name $ mkLongident $ pure name) constructors
  checkDuplicates DuplicateConstructor constrNames

  let typeParamTys = TVar <$> typeParamNames
      qualifiedTypeName = qualifyTypeName moduleName typeName
      resultType = TConstr qualifiedTypeName typeParamTys

  (constrEnvs, constrsTyped) <-
    unzip
      <$> traverse (inferLConDecl moduleName qualifiedTypeName resultType typeParamNames tyConstrEnv) constructors
  pure (TyEnv $ HashMap.unions constrEnvs, constrsTyped)

inferLConDecl ::
  Maybe (LLongident LmlcMr) ->
  Name ->
  Ty ->
  [Name] ->
  TyConstrEnv ->
  LConDecl LmlcMr ->
  MonadTypecheck (HashMap Name TyScheme, LConDecl LmlcTc)
inferLConDecl moduleName typeName resultType typeParamNames tyConstrEnv (L loc conDecl) =
  over _2 (L loc) <$> inferConDecl moduleName typeName resultType typeParamNames tyConstrEnv conDecl

buildConstrType :: [Ty] -> Ty -> Ty
buildConstrType [] resultType = resultType
buildConstrType [single] resultType = single `TArrow` resultType
buildConstrType (hd : tl) resultType = TTuple hd (NE.fromList tl) `TArrow` resultType

inferConDecl ::
  Maybe (LLongident LmlcMr) ->
  Name ->
  Ty ->
  [Name] ->
  TyConstrEnv ->
  ConDecl LmlcMr ->
  MonadTypecheck (HashMap Name TyScheme, ConDecl LmlcTc)
inferConDecl moduleName _typeName resultType typeParamNames tyConstrEnv (ConDecl _ (L loc constrName) argTypes) = do
  let typeParamEnv = HashMap.fromList $ zip typeParamNames (TVar <$> typeParamNames)
  (argTys, argTysTyped) <- unzip <$> traverse (lLmlTypeToTyStrict typeParamEnv tyConstrEnv) argTypes
  let constrType = buildConstrType argTys resultType
      constrScheme = Forall typeParamNames constrType
      constrFullName = case moduleName of
        Just (L _ (Longident modNameNE)) -> Name $ Longident $ modNameNE <> pure constrName
        Nothing -> Name $ mkLongident $ pure constrName
      constrEnv = HashMap.singleton constrFullName constrScheme
  pure (constrEnv, ConDecl noExtField (L loc constrName) argTysTyped)
