{-# LANGUAGE RecursiveDo #-}

module Lamagraph.Compiler.Core.AnfToLllvm (aBindToLlvm) where

import Relude

import Data.HashMap.Strict qualified as HashMap
import Text.LLVM
import Text.LLVM.Triple

import Data.List qualified as List
import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.Anf
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.TcTypes

aExprToLlvm :: HashMap Var (Typed Value) -> AExpr -> BB (Typed Value)
aExprToLlvm m = \case
  ALet (ANonRec var compExpr) body -> do
    varBodyBB <- compExprToLlvm m compExpr
    aExprToLlvm (HashMap.insert var varBodyBB m) body
  ALet (ARec _) _ -> error "Should not exists rec let"
  ACompExpr e -> compExprToLlvm m e

immExprToLlvm :: HashMap Var (Typed Value) -> ImmExpr -> BB (Typed Value)
immExprToLlvm m = \case
  ImmVar name -> pure $ case HashMap.lookup name m of
    Nothing -> error "Trying to access not bound var"
    Just x -> x
  ImmLit (LitInt lit) -> pure $ iT 64 -: lit
  ImmLit _ -> undefined
  ImmTuple{} -> undefined
  ImmLam{} -> error "Must be handled in AExpr"

compExprToLlvm :: HashMap Var (Typed Value) -> CompExpr -> BB (Typed Value)
compExprToLlvm m = \case
  CompApp (ImmVar ((Id (Name (Longident ("+" :| [])))))) (arg1 :| [arg2]) -> do
    arg1BB <- (immExprToLlvm m arg1)
    arg2BB <- (immExprToLlvm m arg2)
    add arg1BB arg2BB
  CompApp f args -> do
    fLlvm <- immExprToLlvm m f
    argsLLvml <- mapM (immExprToLlvm m) (toList args)
    call fLlvm argsLLvml
  CompMatch casedExpr var alts -> case alts of
    (DataAlt (Name (Longident ("true" :| []))), [], trueBody)
      :| [(DataAlt (Name (Longident ("false" :| []))), [], falseBody)] -> do
        rec casedExprBB <- immExprToLlvm m casedExpr
            ite casedExprBB trueLabel falseLabel

            trueLabel <- freshLabel
            label trueLabel
            trueBB <- aExprToLlvm m trueBody
            jump "iteOut"

            falseLabel <- freshLabel
            label falseLabel
            falseBB <- aExprToLlvm m falseBody
            jump "iteOut"

        "iteOut"
        phi (iT 64) [trueBB `from` Named trueLabel, falseBB `from` Named falseLabel]
    _ -> error "Only if's are supported"
  CompImmExpr expr -> immExprToLlvm m expr

ite :: (IsValue a) => Typed a -> Ident -> Ident -> BB ()
ite val trueIdent falseIdent = switch val trueIdent [(0, falseIdent)]

aBindToLlvm :: HashMap Var (Typed Value) -> ABind -> LLVM (Typed Value, HashMap Var (Typed Value))
aBindToLlvm m = \case
  ANonRec var b ->
    case b of
      CompImmExpr immE -> do
        let (args, body) = accumulateLambdas [] immE
            argsLlvm = map (const $ iT 64) args
        varFunc <- define' emptyFunAttrs (iT 64) (symbolByVar var) argsLlvm False $ \arguments -> do
          let newArgs = List.foldl (\hashMap (argVar, llvmArgVar) -> HashMap.insert argVar llvmArgVar hashMap) m (zip args arguments)
          aExprToLlvm newArgs body >>= ret
        pure (varFunc, HashMap.insert var varFunc m)
      -- 0-arity function
      compE -> do
        varFunc <- define emptyFunAttrs (iT 64) (symbolByVar var) () $ do
          compLlvm <- compExprToLlvm m compE
          ret compLlvm
        return (varFunc, HashMap.insert var varFunc m)
  _ -> undefined
 where
  symbolByVar ((Id (Name (Longident v)))) = Symbol $ show v
  symbolByVar _ = error "nonsense"

accumulateLambdas :: [Var] -> ImmExpr -> ([Var], AExpr)
accumulateLambdas args = \case
  ImmLam v (ACompExpr (CompImmExpr e)) -> accumulateLambdas (v : args) e
  ImmLam v e -> (v : args, e)
  e -> ([], ACompExpr $ CompImmExpr e)
