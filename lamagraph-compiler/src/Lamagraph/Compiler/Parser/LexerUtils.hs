{- | Module with functions for working with lexer token stream

For now they are used only in tests.
-}
module Lamagraph.Compiler.Parser.LexerUtils (scanner, getTokenTypes, getTokenTypesFromText) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Parser.Lexer
import Lamagraph.Compiler.Parser.LexerTypes
import Lamagraph.Compiler.Parser.SrcLoc

scanner :: Text -> Either String [LToken]
scanner text = runAlex text loop
 where
  loop = do
    readToken <- alexMonadScan
    if readToken ^. _L . _2 == TokEOF
      then return [readToken]
      else do
        tokens <- loop
        return (readToken : tokens)

getTokenTypes :: Either String [LToken] -> Either String [Token]
getTokenTypes tokens = tokens & _Right %~ toListOf (traverse . _L . _2)

getTokenTypesFromText :: Text -> Either String [Token]
getTokenTypesFromText = getTokenTypes . scanner
