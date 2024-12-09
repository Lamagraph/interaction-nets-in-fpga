-- | Module with initial configuration. It will generate (by TemplateHaskell?) automatically in the future
module Core.Concrete.Initial where

import Clash.Prelude
import Core.Node
import INet.Net

{- | Initial Net view of \((\lambda x. x)(\lambda y. y)\)

<<docs/apply.svg>>
-}
initialIdApplyToId :: Vec (2 ^ BitSize AddressNumber) (Maybe (Node 2))
initialIdApplyToId = Just applyNode +>> Just abstract1Node +>> Just abstract2Node +>> def
 where
  applyAddressNumber = 0
  abstract1AddressNumber = 1
  abstract2AddressNumber = 2
  applyNode =
    let prPort = Port abstract2AddressNumber Primary
        port1 = Port abstract1AddressNumber Primary
        secPorts = Just NotConnected :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Apply
  abstract1Node =
    let prPort = Port applyAddressNumber (Id 1)
        port0 = Port abstract1AddressNumber (Id 1)
        port1 = Port abstract1AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Abstract
  abstract2Node =
    let prPort = Port applyAddressNumber Primary
        port0 = Port abstract2AddressNumber (Id 1)
        port1 = Port abstract2AddressNumber (Id 0)
        secPorts = Just (Connected port0) :> Just (Connected port1) :> Nil
     in Node (Just prPort) secPorts Abstract
