module Lamagraph.Compiler.Parser.LexerUtils (scanner) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Parser.Lexer
import Lamagraph.Compiler.Parser.LexerTypes

scanner :: Text -> Either String [Token]
scanner text = runAlex text loop
 where
  loop = do
    readToken <- alexMonadScan
    if readToken ^. tokenType == TokEOF
      then return [readToken]
      else do
        tokens <- loop
        return (readToken : tokens)
