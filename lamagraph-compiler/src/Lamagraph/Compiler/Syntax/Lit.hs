module Lamagraph.Compiler.Syntax.Lit where

import Relude

import Lamagraph.Compiler.Syntax.Extension

data LmlLit pass
  = LmlInt (XLmlInt pass) Int
  | LmlInt32 (XLmlInt32 pass) Int32
  | LmlUInt32 (XLmlUInt32 pass) Word32
  | LmlInt64 (XLmlInt64 pass) Int64
  | LmlUInt64 (XLmlUInt64 pass) Word64
  | LmlChar (XLmlChar pass) Char
  | LmlString (XLmlString pass) Text
  | XLit !(XXLit pass)
