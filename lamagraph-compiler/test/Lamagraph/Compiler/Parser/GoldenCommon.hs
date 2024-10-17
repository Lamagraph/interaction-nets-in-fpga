module Lamagraph.Compiler.Parser.GoldenCommon (
  renderPretty,
  parserGoldenTestsDir,
  lmlExt,
  changeFileDir,
) where

import Relude

import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath

renderPretty :: Doc ann -> LText
renderPretty = renderLazy . layoutPretty (defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0})

parserGoldenTestsDir :: FilePath
parserGoldenTestsDir = "test" </> "parserGolden" </> "source"

lmlExt :: FilePath
lmlExt = ".lml"

changeFileDir :: FilePath -> FilePath -> FilePath
changeFileDir filePath relativePath = newDir </> fileName
 where
  dir = takeDirectory filePath
  newDir = normalise (dir </> relativePath)
  fileName = takeFileName filePath
