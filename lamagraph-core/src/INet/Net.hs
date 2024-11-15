module INet.Net where

import Clash.Prelude

data AgentSimpleLambda
  = Apply
  | Abs
  deriving (NFDataX, Generic, Show, Eq)

data AgentCombinator
  = Delta
  | Gamma
  | Eps
  deriving (NFDataX, Generic, Show, Eq)

-- It is kind of hack. We need do it smarter later
type Agent = AgentSimpleLambda
