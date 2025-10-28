module Lamagraph.Compiler.ModuleResolver.DefaultEnv where

import Relude

import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.Syntax.Longident

defaultModuleRegistry :: ModuleRegistry
defaultModuleRegistry =
  ModuleRegistry $
    HashMap.fromList
      [
        ( ModulePath $ mkLongident $ pure "#std"
        , HashSet.fromList
            [ "+"
            , "-"
            , "*"
            , "/"
            , ">"
            , "<="
            , "[]"
            , "::"
            , "None"
            , "Some"
            , "true"
            , "false"
            , "~-"
            , "print_int"
            ]
        )
      ]

defaultModuleEnv :: ModuleEnv
defaultModuleEnv =
  ModuleEnv
    { _currentModule = ModulePath $ mkLongident $ pure "#std"
    , _moduleRegistry = defaultModuleRegistry
    , _currentNames = HashSet.empty
    , _localNames = HashSet.empty
    , _opens = []
    }
