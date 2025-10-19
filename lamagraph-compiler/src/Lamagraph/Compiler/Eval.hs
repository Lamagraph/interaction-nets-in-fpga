module Lamagraph.Compiler.Eval (MonadEval, Eval, EvalEnv (..), EvalMock, evalCoreBinds, defEvalEnv, evalCoreBindsDefEnv) where

import Relude hiding (modifyIORef', newIORef, readIORef)

import Data.HashMap.Lazy qualified as HashMap
import Data.Text qualified as T
import UnliftIO

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

data UnaryPrim = UPMinus | UPPrintInt deriving (Show)

data BinaryPrim = BPPlus | BPMinus | BPTimes | BPLess deriving (Show)

data Value
  = VInt Int
  | VChar Char
  | VString Text
  | VClosure Var CoreExpr EvalEnv
  | VUnaryPrim UnaryPrim
  | VBinaryPrim1 BinaryPrim
  | VBinaryPrim2 BinaryPrim Value
  | VTuple Value (NonEmpty Value)
  | VAdt DataCon [Value]
  deriving (Show)

newtype EvalEnv = EvalEnv (HashMap Var Value) deriving (Show)

data EvalException
  = EUnboundVariable Var
  | EInvalidUnaryApply UnaryPrim Value
  | EInvalidBinaryApply BinaryPrim Value Value
  | EInvalidValueApply Value Value
  | EManyLetRecs
  | EMatchFailed
  | ENonLambdaUnderLetRec
  deriving (Show, Typeable)
instance Exception EvalException

class (MonadUnliftIO m) => MonadEval m where
  evalPrint :: String -> m ()

instance MonadEval (IdentityT IO) where
  evalPrint :: String -> IdentityT IO ()
  evalPrint = putStr

type Eval = IdentityT IO

instance MonadEval (ReaderT (IORef Text) IO) where
  evalPrint :: String -> ReaderT (IORef Text) IO ()
  evalPrint msg = do
    ref <- ask
    modifyIORef' ref (`T.append` T.pack msg)

type EvalMock = (ReaderT (IORef Text) IO)

evalCoreExpr :: (MonadEval m) => EvalEnv -> CoreExpr -> m Value
evalCoreExpr eEnv@(EvalEnv env) = \case
  Var name -> case HashMap.lookup name env of
    Just val -> pure val
    Nothing -> throwIO $ EUnboundVariable name
  Lit lit -> case lit of
    LitInt int -> pure $ VInt int
    LitChar char -> pure $ VChar char
    LitString str -> pure $ VString str
  App f arg -> do
    fVal <- evalCoreExpr eEnv f
    argVal <- evalCoreExpr eEnv arg
    evalApply fVal argVal
  Lam var expr -> pure $ VClosure var expr eEnv
  Let bind expr -> do
    newEnv <- evalCoreBind eEnv bind
    evalCoreExpr newEnv expr
  Match expr var alts -> do
    val <- evalCoreExpr eEnv expr
    let newEnv :: EvalEnv = coerce $ HashMap.insert var val (coerce eEnv)
    matchAlts newEnv val $ toList alts
  Tuple val vals -> VTuple <$> evalCoreExpr eEnv val <*> mapM (evalCoreExpr eEnv) vals

evalApply :: (MonadEval m) => Value -> Value -> m Value
evalApply fVal argVal = case fVal of
  VClosure cVar cExpr cEnv -> evalCoreExpr (coerce $ HashMap.insert cVar argVal (coerce cEnv)) cExpr
  VUnaryPrim op -> evalUnaryPrim op argVal
  VBinaryPrim1 op -> pure $ VBinaryPrim2 op argVal
  VBinaryPrim2 op arg1 -> evalBinaryPrim op arg1 argVal
  -- Constructors must be fully applied!
  VAdt conName [] -> case argVal of
    VTuple val vals -> pure $ VAdt conName (val : toList vals)
    val -> pure $ VAdt conName [val]
  _ -> throwIO $ EInvalidValueApply fVal argVal

evalUnaryPrim :: (MonadEval m) => UnaryPrim -> Value -> m Value
evalUnaryPrim UPMinus (VInt arg) = pure $ VInt $ -arg
evalUnaryPrim UPPrintInt (VInt arg) = do
  (evalPrint . show) arg
  pure $ VAdt (Name $ mkLongident $ pure "()") []
evalUnaryPrim prim arg = throwIO $ EInvalidUnaryApply prim arg

evalBinaryPrim :: (MonadEval m) => BinaryPrim -> Value -> Value -> m Value
evalBinaryPrim BPPlus (VInt arg1) (VInt arg2) = pure $ VInt $ arg1 + arg2
evalBinaryPrim BPMinus (VInt arg1) (VInt arg2) = pure $ VInt $ arg1 - arg2
evalBinaryPrim BPTimes (VInt arg1) (VInt arg2) = pure $ VInt $ arg1 * arg2
evalBinaryPrim BPLess (VInt arg1) (VInt arg2) =
  let val = if arg1 < arg2 then "true" else "false"
   in pure $ VAdt (Name $ mkLongident $ pure val) []
evalBinaryPrim prim arg1 arg2 = throwIO $ EInvalidBinaryApply prim arg1 arg2

{- | I think this is a bad code, because it can force pattern-matching in weird places

For example

@
let f x = match x with 1 -> \'1\' | 2 -> \'2\'
let h x = match x with \'V\' -> f 5 | \'I\' -> f 1
let v = h \'V\'
@

We know that this code will produce 'EMatchFailed' inside @f 5@,
but after it will be caught one level above, inside @h \'V\'@, we'll also
try and match \'V\' with \'I\'.

This behavior at least will slow down interpretation, but can cause some trouble,
e.g. if we have @h@ from above defined as

@
let h x = match x with \'V\' -> f 5 | \'V\' -> f 1
@

Still, thats the only way I currently know to work with nested matches.
-}
matchAlt :: (MonadEval m) => EvalEnv -> Value -> CoreMatchAlt -> m Value
matchAlt eEnv = \cases
  (VAdt valConName valConArgs) (DataAlt altConName, altConArgs, altExpr) ->
    let argsForEnv = zip altConArgs valConArgs
        newEnv = coerce $ HashMap.fromList argsForEnv `HashMap.union` coerce eEnv
     in if altConName == valConName then evalCoreExpr newEnv altExpr else throwIO EMatchFailed
  (VTuple valVal valVals) (TupleAlt, altArgs, altExpr) ->
    let valsList = valVal : toList valVals
        argsForEnv = zip altArgs valsList
        newEnv = coerce $ HashMap.fromList argsForEnv `HashMap.union` coerce eEnv
     in if length valsList == length altArgs
          then evalCoreExpr newEnv altExpr
          else throwIO EMatchFailed
  (VInt val) (LitAlt (LitInt altArg), [], altExpr) -> if val == altArg then evalCoreExpr eEnv altExpr else throwIO EMatchFailed
  (VChar val) (LitAlt (LitChar altArg), [], altExpr) -> if val == altArg then evalCoreExpr eEnv altExpr else throwIO EMatchFailed
  (VString val) (LitAlt (LitString altArg), [], altExpr) -> if val == altArg then evalCoreExpr eEnv altExpr else throwIO EMatchFailed
  _ (DEFAULT, [], altExpr) -> evalCoreExpr eEnv altExpr
  _ _ -> throwIO EMatchFailed

matchAlts :: (MonadEval m) => EvalEnv -> Value -> [CoreMatchAlt] -> m Value
matchAlts eEnv val = \case
  [] -> throwIO EMatchFailed
  alt : alts ->
    matchAlt eEnv val alt `catch` \case
      EMatchFailed -> matchAlts eEnv val alts
      err -> throwIO err

evalCoreBind :: (MonadEval m) => EvalEnv -> CoreBind -> m EvalEnv
evalCoreBind eEnv = \case
  NonRec var expr -> do
    value <- evalCoreExpr eEnv expr
    pure $ coerce $ HashMap.insert var value (coerce eEnv)
  Rec (bind :| []) ->
    let (funVar, lamVar, lamExpr) = case bind of
          (var, Lam lamVar' lamExpr') -> (var, lamVar', lamExpr')
          _ -> impureThrow ENonLambdaUnderLetRec
        recEnv = coerce $ HashMap.insert funVar (VClosure lamVar lamExpr recEnv) (coerce eEnv)
        value = VClosure lamVar lamExpr recEnv
     in pure $ coerce $ HashMap.insert funVar value (coerce eEnv)
  Rec (_ :| _) -> throwIO EManyLetRecs

evalCoreBinds :: (MonadEval m) => EvalEnv -> [CoreBind] -> m EvalEnv
evalCoreBinds = foldlM evalCoreBind

defEvalEnv :: EvalEnv
defEvalEnv =
  EvalEnv $
    HashMap.fromList
      [ (Id $ Name $ mkLongident $ pure "~-", VUnaryPrim UPMinus)
      , (Id $ Name $ mkLongident $ pure "+", VBinaryPrim1 BPPlus)
      , (Id $ Name $ mkLongident $ pure "-", VBinaryPrim1 BPMinus)
      , (Id $ Name $ mkLongident $ pure "*", VBinaryPrim1 BPTimes)
      , (Id $ Name $ mkLongident $ pure "<", VBinaryPrim1 BPLess)
      , (Id $ Name $ mkLongident $ pure "[]", VAdt (Name $ mkLongident $ pure "[]") [])
      , (Id $ Name $ mkLongident $ pure "::", VAdt (Name $ mkLongident $ pure "::") [])
      , (Id $ Name $ mkLongident $ pure "Some", VAdt (Name $ mkLongident $ pure "Some") [])
      , (Id $ Name $ mkLongident $ pure "None", VAdt (Name $ mkLongident $ pure "None") [])
      , (Id $ Name $ mkLongident $ pure "print_int", VUnaryPrim UPPrintInt)
      ]

evalCoreBindsDefEnv :: (MonadEval m) => [CoreBind] -> m EvalEnv
evalCoreBindsDefEnv = evalCoreBinds defEvalEnv
