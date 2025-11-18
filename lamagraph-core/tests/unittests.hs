import Prelude

import Test.Tasty
import qualified Tests.Core.Node
import Tests.Core.Unit.CPU
import Tests.Core.Unit.Loader
import Tests.Core.Unit.MemoryManager.MemoryManager
import Tests.Core.Unit.Reducer
import Tests.GraphRewriting.SKI

main :: IO ()
main = do
  skiUnitTests' <- skiUnitTests
  defaultMain $
    testGroup
      " Unit tests"
      [ testGroup
          "Clash tests"
          [ Tests.Core.Node.accumTests
          , reducerUnitTests
          , memoryManagerUnitTests
          , mealyCoreUnitTests
          , loaderUnitTests
          ]
      , testGroup
          "Graph Rewriting translator tests"
          [ skiUnitTests'
          ]
      ]
