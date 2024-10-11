{-# LANGUAGE TypeFamilies #-}

module Lamagraph.Compiler.Extension where

import Relude

import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Passes
import Lamagraph.Compiler.Syntax

type instance XLocated (LmlcPass p) a = Located a

type instance XCModule (LmlcPass _) = NoExtField
type instance XXModule (LmlcPass _) = DataConCantHappen

type instance XOpenD (LmlcPass _) = NoExtField
type instance XXLmlDecl (LmlcPass _) = DataConCantHappen

type instance XOpenDecl (LmlcPass _) = NoExtField
type instance XXOpenDecl (LmlcPass _) = DataConCantHappen
