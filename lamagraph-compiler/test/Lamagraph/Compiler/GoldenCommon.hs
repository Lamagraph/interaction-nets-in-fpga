module Lamagraph.Compiler.GoldenCommon (
  renderPretty,
  baseGoldenTestsDir,
  parserSourceGoldenTestsDir,
  typecheckerSourceGoldenTestsDir,
  typecheckerSourceGoldenModuleTestsDir,
  coreSourceGoldenTestsDir,
  evalSourceGoldenTestsDir,
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

baseGoldenModuleTestsDir :: FilePath
baseGoldenModuleTestsDir = "test" </> "golden-module"

parserSourceGoldenTestsDir :: FilePath
parserSourceGoldenTestsDir = baseGoldenTestsDir </> "parser" </> "source"

typecheckerSourceGoldenTestsDir :: FilePath
typecheckerSourceGoldenTestsDir = baseGoldenTestsDir </> "typechecker" </> "source"

typecheckerSourceGoldenModuleTestsDir :: FilePath
typecheckerSourceGoldenModuleTestsDir = baseGoldenModuleTestsDir </> "typechecker" </> "source"

coreSourceGoldenTestsDir :: FilePath
coreSourceGoldenTestsDir = baseGoldenTestsDir </> "core" </> "source"

evalSourceGoldenTestsDir :: FilePath
evalSourceGoldenTestsDir = baseGoldenTestsDir </> "eval" </> "source"

lmlExt :: FilePath
lmlExt = ".lml"

changeFileDir :: FilePath -> FilePath -> FilePath
changeFileDir filePath relativePath = newDir </> fileName
 where
  dir = takeDirectory filePath
  newDir = normalise (dir </> relativePath)
  fileName = takeFileName filePath
