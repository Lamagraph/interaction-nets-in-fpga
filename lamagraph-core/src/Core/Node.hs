{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, set, (^.))
import INet.Net

{- | Type alias for `Maybe Port` with aliased constructors too.
This can be useful for distinguish connected to something `Port`, free `Port` that is not connected and void `Port`.
The last one come from situation when max number of ports (i.e. max arity) greater than arity of the given `Node`.
-}
type Connection (portsNumber :: Nat) = Maybe (Port portsNumber)

pattern Connected :: Port portsNumber -> Connection portsNumber
pattern Connected x = Just x

pattern NotConnected :: Connection portsNumber
pattern NotConnected = Nothing
{-# COMPLETE Connected, NotConnected #-}

type AddressNumber = Unsigned 16

type LocalAddressNumber = AddressNumber
type ActualAddressNumber = AddressNumber

data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary
  deriving (Generic, Show, Eq, NFDataX)

data Port (portsNumber :: Nat) = Port
  { _nodeAddress :: AddressNumber
  , _portConnectedToId :: IdOfPort portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''Port)

-- | Node in the RAM.
data Node portsNumber = Node
  { _primaryPort :: Connection portsNumber
  , _secondaryPorts :: Vec portsNumber (Maybe (Connection portsNumber))
  , _nodeType :: Agent --  looks like we need some kind of node label. Info about and reduction rules contained IN
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''Node)

{- | `Node` with info about his`Address`.
Original address can be useful when reducer working.
For example, if this `Node` has reduced then his `Address` is become free
and info about this should be passed to the memory manager.
-}
data LoadedNode (portsNumber :: Nat) = LoadedNode
  { _containedNode :: Node portsNumber
  , _originalAddress :: ActualAddressNumber
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''LoadedNode)

{- | Check if pair of `LoadedNode` are active, i.e. they are connected by primary ports.
| Check if `Node` is active
-}
nodeIsActive ::
  (KnownNat portsNumber) => Node portsNumber -> Bool
nodeIsActive node =
  case node ^. primaryPort of
    Just port -> case port ^. portConnectedToId of
      Primary -> True
      _ -> False
    _ -> False

setConnection ::
  (KnownNat portsNumber) =>
  Node portsNumber ->
  IdOfPort portsNumber ->
  Connection portsNumber ->
  Node portsNumber
setConnection node portId connection = case portId of
  Primary -> set primaryPort connection node
  Id index -> set secondaryPorts (replace index (Just connection) (node ^. secondaryPorts)) node
