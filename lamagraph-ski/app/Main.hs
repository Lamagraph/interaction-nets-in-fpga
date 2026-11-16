{- | Visual-debugging driver for the YALE reducer. Builds a YALE interaction
net (from a built-in example or an LML source file), reduces it step by step,
and writes one INviz JSON file per step into an output directory. Render the
directory with INviz (@python3 INviz/main.py <dir>@) to see the reduction as
a sequence of pictures.

Usage:

    yale-draw <example|file.lml> <outdir> [maxSteps]

Built-in examples (Church terms, the decode net @term tick base@):

    pred1    predNonSat 1        (simplest broken case: want 0)
    pred2    predNonSat 2        (want 1)
    sub82    sub 8 2             (want 6)
    min3a528 min3a 5 2 8         (the #104 failing case: want 2)

Otherwise the first argument is treated as an LML source file; the net for the
program's result applied to the @__tick__@/@__base__@ decorators is traced.
-}
module Main (main) where

import Relude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GraphRewriting (Graph)
import Text.Read (read)
import Lamagraph.Compiler.Core (Bind (NonRec), CoreBind, CoreExpr)
import Lamagraph.Compiler.Core.MonadDesugar (runMonadDesugar)
import Lamagraph.Compiler.Driver (
  desugarLmlProgram,
  parseLmlProgram,
  resolveLmlProgram,
  typecheckLmlProgram,
 )
import Lamagraph.Ski.Church (churchLam, predLam, subLam)
import Lamagraph.Ski.Lambda (Lam (..))
import Lamagraph.Ski.Translate (translateLam)
import Lamagraph.Ski.YaleDraw (dumpReduction)
import Lamagraph.Ski.YaleNet (YaleNode, buildYaleNet)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [what, outdir] -> run what outdir 100000
    [what, outdir, steps] -> run what outdir (read steps)
    _ -> do
      putStrLn "usage: yale-draw <example|file.lml> <outdir> [maxSteps]"
      exitFailure
 where
  run :: String -> FilePath -> Int -> IO ()
  run what outdir maxSteps = do
    result <- buildNet what
    case result of
      Left err -> do
        putStrLn ("error: " ++ err)
        exitFailure
      Right g -> do
        dumpReduction outdir maxSteps g
        putStrLn ("wrote reduction steps to " ++ outdir)

-- | Build the YALE net (the decode net @term tick base@) for a built-in
-- example name or an LML source file.
buildNet :: String -> IO (Either String (Graph YaleNode))
buildNet what = case builtIn what of
  Just lam -> return (Right (decodeNet lam))
  Nothing -> do
    src <- T.unpack . TE.decodeUtf8 <$> BS.readFile what
    case compileLml src of
      Left err -> return (Left err)
      Right (binds, expr) ->
        case translateLam binds expr of
          Left err -> return (Left (show err))
          Right lam -> return (Right (decodeNet lam))

-- | Apply the @__tick__@/@__base__@ decorators (the decode net that
-- 'Lamagraph.Ski.YaleDecode' reduces).
decodeNet :: Lam -> Graph YaleNode
decodeNet lam = buildYaleNet (LApp (LApp lam (LVar "__tick__")) (LVar "__base__"))

-- | The built-in examples, as closed Church terms.
builtIn :: String -> Maybe Lam
builtIn name = case name of
  "pred1" -> Just (LApp (evalState predLam 0) (num 1))
  "pred2" -> Just (LApp (evalState predLam 0) (num 2))
  "sub82" -> Just (LApp (LApp (evalState subLam 0) (num 8)) (num 2))
  "min3a528" -> min3aNet 5 2 8
  _ -> Nothing
 where
  num :: Int -> Lam
  num k = evalState (churchLam k) 0

-- | The @min3a@ (if-based min of three) decode net for given literals.
min3aNet :: Integer -> Integer -> Integer -> Maybe Lam
min3aNet x y z = case compileLml (min3aSrc x y z) of
  Left _ -> Nothing
  Right (binds, expr) -> case translateLam binds expr of
    Left _ -> Nothing
    Right lam -> Just lam

min3aSrc :: Integer -> Integer -> Integer -> String
min3aSrc x y z =
  "module M\nlet min a b = if a < b then a else b\n"
    ++ "let min3a p q r = if p < q then (if p < r then p else r) else (if q < r then q else r)\n"
    ++ "let result = min3a " ++ show x ++ " " ++ show y ++ " " ++ show z

-- | Compile an LML source to its core bindings and result expression.
compileLml :: String -> Either String ([CoreBind], CoreExpr)
compileLml src = case parseLmlProgram [T.pack src] of
  Left e -> Left ("parse: " ++ show e)
  Right parsed ->
    let resolved = resolveLmlProgram parsed
     in case typecheckLmlProgram resolved of
          Left e -> Left ("typecheck: " ++ show e)
          Right typed ->
            let binds = runMonadDesugar (desugarLmlProgram typed)
             in case nonEmpty binds of
                  Nothing -> Left "empty program"
                  Just neb ->
                    let rest = toList (init neb)
                        final = last neb
                     in case final of
                          NonRec _ e -> Right (rest, e)
                          _ -> Left "last binding is recursive"
