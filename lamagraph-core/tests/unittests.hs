import Prelude

import Test.Tasty
import qualified Tests.Core.Node

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Core.Node.accumTests
      ]
