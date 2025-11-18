module Lamagraph.Compiler.ModuleResolver.Helper (prependModuleName, lookupName, resolveMany) where

import Relude

import Control.Lens
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet
import Data.List.NonEmpty as NonEmpty

import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.Syntax.Longident

prependModuleName :: ModulePath -> Text -> FullName
prependModuleName path name = FullName $ Longident $ coerce path <> NonEmpty.singleton name

lookupInModule :: ModuleRegistry -> ModulePath -> Text -> Maybe FullName
lookupInModule (ModuleRegistry reg) path name = do
  moduleNames <- HashMap.lookup path reg
  if member name moduleNames
    then Just $ prependModuleName path name
    else Nothing

lookupUnqualified :: ModuleEnv -> Text -> Maybe FullName
lookupUnqualified env name =
  lookupLocal <|> lookupOpens <|> lookupCurrent
 where
  -- local and current module bindings get their current module name prepended
  lookupLocal = view localNames env & find (== name) <&> prependModuleName (coerce (env ^. currentModule))
  lookupCurrent =
    view currentNames env
      & find (== name)
      <&> prependModuleName (coerce (env ^. currentModule))
  lookupOpens = join $ find isJust [lookupInModule (env ^. moduleRegistry) open name | open <- env ^. opens]

lookupQualified :: ModuleEnv -> Longident -> Maybe FullName
lookupQualified env (Longident nameNE) =
  let valName = NonEmpty.last nameNE
      modParts = NonEmpty.init nameNE
      modPath = ModulePath $ Longident $ NonEmpty.fromList modParts
   in lookupInModule (env ^. moduleRegistry) modPath valName

lookupName :: ModuleEnv -> Longident -> Maybe FullName
lookupName env name@(Longident nameNE) = case NonEmpty.init nameNE of
  [] -> lookupUnqualified env (NonEmpty.last nameNE)
  _ -> lookupQualified env name

resolveMany :: (Foldable t1, Monad m) => t2 -> (t2 -> t3 -> m (t2, a)) -> t1 t3 -> m (t2, [a])
resolveMany env resolver =
  foldlM
    ( \(e, acc) p -> do
        (e', rp) <- resolver e p
        pure (e', acc <> [rp])
    )
    (env, [])
