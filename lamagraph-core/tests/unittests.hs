import Prelude

import Test.Tasty
import qualified Tests.Core.Node
import Tests.Core.Unit.Reducer

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Core.Node.accumTests
      , reducerUnitTests
      ]
