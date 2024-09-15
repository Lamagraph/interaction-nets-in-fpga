module Lamagraph.Compiler.Parser.LexerUtils where

import Lamagraph.Compiler.Parser.Lexer
import Relude

scanner :: Text -> Either String [Token]
scanner text = runAlex text loop
 where
  loop = do
    readToken <- alexMonadScan
    if _tokenType readToken == TokEOF
      then return [readToken]
      else do
        tokens <- loop
        return (readToken : tokens)
