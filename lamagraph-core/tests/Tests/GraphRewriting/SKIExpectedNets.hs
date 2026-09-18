module Tests.GraphRewriting.SKIExpectedNets where

import Clash.Prelude
import Core.Node
import GraphRewriting.SKI
import INet.Net

bluebirdINet :: Net 2 SKINet
bluebirdINet =
  Just
    ( Node
        { _primaryPort = Just (Port{_nodeAddress = 1, _portConnectedToId = Primary})
        , _secondaryPorts =
            Just (Just (Port{_nodeAddress = 5, _portConnectedToId = Primary}))
              :> Just (Just (Port{_nodeAddress = 2, _portConnectedToId = Id 0}))
              :> Nil
        , _nodeType = INA
        }
    )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 0, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 3, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 0, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 4, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 2, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 2, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 0, _portConnectedToId = Id 0})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 7, _portConnectedToId = Primary}))
                :> Just (Just (Port{_nodeAddress = 6, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 5, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      )
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 5, _portConnectedToId = Id 0})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INR
          }
      )
    :> def

cardinalINet :: Net 2 SKINet
cardinalINet =
  Just
    ( Node
        { _primaryPort = Just (Port{_nodeAddress = 1, _portConnectedToId = Primary})
        , _secondaryPorts =
            Just (Just (Port{_nodeAddress = 15, _portConnectedToId = Primary}))
              :> Just (Just (Port{_nodeAddress = 13, _portConnectedToId = Id 0}))
              :> Nil
        , _nodeType = INA
        }
    ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 0, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 3, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 13, _portConnectedToId = Primary}))
                :> Just (Just (Port{_nodeAddress = 4, _portConnectedToId = Id 0}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 2, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 5, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 2, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 11, _portConnectedToId = Id 0}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 4, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 7, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 11, _portConnectedToId = Primary}))
                :> Just (Just (Port{_nodeAddress = 8, _portConnectedToId = Id 0}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 6, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 9, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 6, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 10, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 8, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 8, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 6, _portConnectedToId = Id 0})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 4, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 12, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 11, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 2, _portConnectedToId = Id 0})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 0, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 14, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 13, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INS0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 0, _portConnectedToId = Id 0})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 19, _portConnectedToId = Primary}))
                :> Just (Just (Port{_nodeAddress = 16, _portConnectedToId = Id 0}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 17, _portConnectedToId = Primary})
          , _secondaryPorts =
              Just (Just (Port{_nodeAddress = 15, _portConnectedToId = Id 1}))
                :> Just (Just (Port{_nodeAddress = 18, _portConnectedToId = Primary}))
                :> Nil
          , _nodeType = INA
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 16, _portConnectedToId = Primary})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 16, _portConnectedToId = Id 1})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INK0
          }
      ) -- x
    :> Just
      ( Node
          { _primaryPort = Just (Port{_nodeAddress = 15, _portConnectedToId = Id 0})
          , _secondaryPorts = Nothing :> Nothing :> Nil
          , _nodeType = INR
          }
      ) -- x
    :> def
