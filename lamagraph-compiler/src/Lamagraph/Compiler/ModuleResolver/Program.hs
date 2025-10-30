module Lamagraph.Compiler.ModuleResolver.Program (LmlProgram (..)) where

import Lamagraph.Compiler.Syntax

newtype LmlProgram pass = LmlProgram [LmlModule pass]
