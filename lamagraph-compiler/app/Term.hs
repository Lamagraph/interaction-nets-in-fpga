module Term where

import Relude

data Expr = A Expr Expr | S | K | I | V String deriving (Ord, Eq, Show)

skk :: Expr
skk = A (A S K) K
