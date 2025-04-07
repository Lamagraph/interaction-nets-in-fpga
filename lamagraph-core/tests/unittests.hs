import Prelude

import Test.Tasty
import qualified Tests.Core.Node
import Tests.Core.Unit.CPU
import Tests.Core.Unit.Core
import Tests.Core.Unit.Loader
import Tests.Core.Unit.MemoryManager.MemoryManager
import Tests.Core.Unit.Reducer

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ Tests.Core.Node.accumTests
      , reducerUnitTests
      , memoryManagerUnitTests
      , -- , coreUnitTests
        mealyCoreUnitTests
      , loaderUnitTests
      ]
