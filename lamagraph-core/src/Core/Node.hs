{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))

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

type AddressBitSize = 4
type AddressNumber = Unsigned AddressBitSize
type CellsNumber = 2 ^ AddressBitSize

data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary
  deriving (Generic, Show, Eq, NFDataX, ShowX)

data Port (portsNumber :: Nat) = Port
  { _nodeAddress :: AddressNumber
  , _portConnectedToId :: IdOfPort portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''Port)

-- | Node in the RAM.
data Node portsNumber agentType = Node
  { _primaryPort :: Connection portsNumber
  , _secondaryPorts :: Vec portsNumber (Maybe (Connection portsNumber))
  , _nodeType :: agentType --  looks like we need some kind of node label. Info about and reduction rules contained IN
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''Node)

{- | `Node` with info about his`Address`.
Original address can be useful when reducer working.
For example, if this `Node` has reduced then his `Address` is become free
and info about this should be passed to the memory manager.
-}
data LoadedNode (portsNumber :: Nat) agentType = LoadedNode
  { _containedNode :: Node portsNumber agentType
  , _originalAddress :: AddressNumber
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''LoadedNode)

data Edge (portsNumber :: Nat) = Edge
  { _leftEnd :: Connection portsNumber
  , _rightEnd :: Connection portsNumber
  }
  deriving (Generic, NFDataX, Show, Eq)

$(makeLenses ''Edge)

-- | Check if `Node` is active
nodeIsActive ::
  (KnownNat portsNumber) => Node portsNumber agentType -> Bool
nodeIsActive node =
  case node ^. primaryPort of
    Just port -> case port ^. portConnectedToId of
      Primary -> True
      _ -> False
    _ -> False

-- | Get `Vec` all `Connection`s of `Node`
getAllConnections ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Vec (portsNumber + 1) (Maybe (Connection portsNumber))
getAllConnections node = Just (node ^. primaryPort) :> (node ^. secondaryPorts)

findConnection ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Connection portsNumber ->
  Maybe (Connection portsNumber)
findConnection (Node primPort secPorts _) = \case
  NotConnected -> Nothing
  Connected (Port _ portId) ->
    case portId of
      Primary -> Just primPort
      Id i -> secPorts !! i
