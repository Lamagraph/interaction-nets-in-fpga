{-# LANGUAGE DataKinds #-}

module Lamagraph.Compiler.Passes where

data Pass = Parsed

data LmlcPass (c :: Pass) where
  LmlcPs :: LmlcPass 'Parsed

type LmlcPs = LmlcPass 'Parsed -- Output of parser
