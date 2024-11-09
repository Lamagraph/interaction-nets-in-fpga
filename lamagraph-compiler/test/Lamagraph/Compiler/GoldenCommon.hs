module Lamagraph.Compiler.GoldenCommon (
  renderPretty,
  parserSourceGoldenTestsDir,
  typecheckerSourceGoldenTestsDir,
  lmlExt,
  changeFileDir,
) where

import Relude

import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath

renderPretty :: Doc ann -> LText
renderPretty = renderLazy . layoutPretty (defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0})

baseGoldenTestsDir :: FilePath
baseGoldenTestsDir = "test" </> "golden"

parserSourceGoldenTestsDir :: FilePath
parserSourceGoldenTestsDir = baseGoldenTestsDir </> "parser" </> "source"

typecheckerSourceGoldenTestsDir :: FilePath
typecheckerSourceGoldenTestsDir = baseGoldenTestsDir </> "typechecker" </> "source"

lmlExt :: FilePath
lmlExt = ".lml"

changeFileDir :: FilePath -> FilePath -> FilePath
changeFileDir filePath relativePath = newDir </> fileName
 where
  dir = takeDirectory filePath
  newDir = normalise (dir </> relativePath)
  fileName = takeFileName filePath
