{-# LANGUAGE MultiParamTypeClasses #-}

module GraphRewriting.SKI where

import qualified Clash.Explicit.Prelude as Clash.Prelude
import qualified GraphRewriting.Translator as Translator
import INet.Graph
import Prelude

data SKINet
  = INR
  | INA
  | INI
  | INE
  | IND
  | INV
  | INK0
  | INK1
  | INS0
  | INS1
  | INS2
  deriving (Clash.Prelude.NFDataX, Clash.Prelude.Generic, Show, Eq, Clash.Prelude.ShowX)

instance Translator.AgentMaps SKI SKINet where
  agentToAgent :: SKI -> SKINet
  agentToAgent = \case
    R{} -> INR
    A{} -> INA
    I{} -> INI
    E{} -> INE
    D{} -> IND
    V{} -> INV
    K0{} -> INK0
    K1{} -> INK1
    S0{} -> INS0
    S1{} -> INS1
    S2{} -> INS2
