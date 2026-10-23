module Lamagraph.Compiler.Parser.ParserRoundtrip (prop_ParserRoundtrip) where

import Relude

import Hedgehog
import Prettyprinter
import Prettyprinter.Render.Text

import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.PrettyLml ()
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.TestGen (genModule)

prop_ParserRoundtrip :: Property
prop_ParserRoundtrip = withTests 100 . property $ do
  asts <- forAll genModule
  tripping asts (renderStrict . layoutPretty defaultLayoutOptions . pretty) parseLamagraphML
