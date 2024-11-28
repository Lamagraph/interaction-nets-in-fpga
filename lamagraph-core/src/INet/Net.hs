module INet.Net where

import Clash.Prelude

data AgentSimpleLambda
  = Apply
  | Abstract
  | Erase
  deriving (NFDataX, Generic, Show, Eq)

-- It is kind of hack. We need do it smarter later
type Agent = AgentSimpleLambda
