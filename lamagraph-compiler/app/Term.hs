module Term (Expr (..), skk) where

import Relude

data Expr = A Expr Expr | S | K | I | V String deriving (Ord, Eq, Show)

skk :: Expr
skk = flip A (V "x") $ A (A S K) K
