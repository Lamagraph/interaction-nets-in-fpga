module Lamagraph.Compiler.Nets.Encodings.LambdaCalculusCore (resFactRecZero, resFactNonRecZero, resFactRecOne, resFactRecTwo) where

import Relude hiding (one, pred)

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

---------------
-- Variables --
---------------

b, f, g, h, m, n, t, u, v, x, y, z :: Var
b = Id $ Name $ mkLongident $ pure "b"
f = Id $ Name $ mkLongident $ pure "f"
g = Id $ Name $ mkLongident $ pure "g"
h = Id $ Name $ mkLongident $ pure "h"
m = Id $ Name $ mkLongident $ pure "m"
n = Id $ Name $ mkLongident $ pure "n"
t = Id $ Name $ mkLongident $ pure "t"
u = Id $ Name $ mkLongident $ pure "u"
v = Id $ Name $ mkLongident $ pure "v"
x = Id $ Name $ mkLongident $ pure "x"
y = Id $ Name $ mkLongident $ pure "y"
z = Id $ Name $ mkLongident $ pure "z"

-----------
-- Bools --
-----------

true :: Var
true = Id $ Name $ mkLongident $ pure "true"
trueB :: CoreBind
trueB = NonRec true $ Lam x $ Lam y $ Var x

false :: Var
false = Id $ Name $ mkLongident $ pure "false"
falseB :: CoreBind
falseB = NonRec false $ Lam x $ Lam y $ Var y

_if :: Var
_if = Id $ Name $ mkLongident $ pure "if"
_ifB :: CoreBind
_ifB = NonRec _if $ Lam b $ Lam t $ Lam f $ App (App (Var b) (Var t)) (Var f)

--------------
-- Numerals --
--------------
zero :: Var
zero = Id $ Name $ mkLongident $ pure "zero"
zeroB :: CoreBind
zeroB = NonRec zero $ Lam f $ Lam x $ Var x

one :: Var
one = Id $ Name $ mkLongident $ pure "one"
oneB :: CoreBind
oneB = NonRec one $ Lam f $ Lam x $ App (Var f) (Var x)

two :: Var
two = Id $ Name $ mkLongident $ pure "two"
twoB :: CoreBind
twoB = NonRec two $ Lam f $ Lam x $ App (Var f) (App (Var f) (Var x))

----------------
-- Arithmetic --
----------------

pred :: Var
pred = Id $ Name $ mkLongident $ pure "pred"
predB :: CoreBind
predB = NonRec pred $ Lam n $ Lam f $ Lam x $ App (App (App (Var n) firstArg) secondArg) thirdArg
 where
  firstArg = Lam g $ Lam h $ App (Var h) (App (Var g) (Var f))
  secondArg = Lam u $ Var x
  thirdArg = Lam u $ Var u

mult :: Var
mult = Id $ Name $ mkLongident $ pure "mult"
multB :: CoreBind
multB = NonRec mult $ Lam m $ Lam n $ Lam f $ Lam x $ App (App (Var m) (App (Var n) (Var f))) (Var x)

isZero :: Var
isZero = Id $ Name $ mkLongident $ pure "isZero"
isZeroB :: CoreBind
isZeroB = NonRec isZero $ Lam n $ App (App (Var n) (Lam x (Var false))) (Var true)

---------------
-- Functions --
---------------

zCombinator :: CoreExpr
zCombinator = Lam f (App inner inner)
 where
  inner = Lam x $ App (Var f) (Lam v (App (App (Var x) (Var x)) (Var v)))

fact :: Var
fact = Id $ Name $ mkLongident $ pure "fact"

factNonRecB :: CoreBind
factNonRecB = NonRec fact $ Lam n $ App (App zCombinator fact') (Var n)
 where
  fact' = Lam f $ Lam n $ App (App (App (Var _if) (App (Var isZero) (Var n))) (Lam z $ Var one)) innerApp
  innerApp = Lam z $ App (App (Var mult) (Var n)) (App (Var f) (App (Var pred) (Var n)))

factRecB :: Bind Var
factRecB = Rec $ pure (fact, Lam n $ App (App (App (Var _if) (App (Var isZero) (Var n))) (Lam z $ Var one)) innerApp)
 where
  innerApp = Lam z $ App (App (Var mult) (Var n)) (App (Var fact) (App (Var pred) (Var n)))

resFactRecZero :: [CoreBind]
resFactRecZero = [trueB, falseB, _ifB, oneB, predB, multB, isZeroB, zeroB, factRecB, res]
 where
  res = NonRec (Id $ Name $ mkLongident $ pure "res") $ App (Var fact) (Var zero)

resFactNonRecZero :: [CoreBind]
resFactNonRecZero = [trueB, falseB, _ifB, oneB, predB, multB, isZeroB, zeroB, factNonRecB, res]
 where
  res = NonRec (Id $ Name $ mkLongident $ pure "res") $ App (Var fact) (Var zero)

resFactRecOne :: [CoreBind]
resFactRecOne = [trueB, falseB, _ifB, oneB, predB, multB, isZeroB, factRecB, res]
 where
  res = NonRec (Id $ Name $ mkLongident $ pure "res") $ App (Var fact) (Var one)

resFactRecTwo :: [CoreBind]
resFactRecTwo = [trueB, falseB, _ifB, oneB, predB, multB, isZeroB, twoB, factRecB, res]
 where
  res = NonRec (Id $ Name $ mkLongident $ pure "res") $ App (Var fact) (Var two)
