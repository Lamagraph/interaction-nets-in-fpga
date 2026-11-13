import Prelude

import Test.Tasty
import qualified Tests.Core.Node
import Tests.Core.Unit.CPU
import Tests.Core.Unit.CycleBudget (cycleBudgetTests)
import Tests.Core.Unit.EdslAgents (edslAgentsTests)
import Tests.Core.Unit.EdslDispatch (edslDispatchTests)
import Tests.Core.Unit.EdslRules (edslRulesTests)
import Tests.Core.Unit.EdslRulesUnit (allocationCountTests, edslRulesUnitTests)
import Tests.Core.Unit.Loader
import Tests.Core.Unit.MemoryManager
import Tests.Core.Unit.Reducer
import Tests.Core.Unit.Symmetry (symmetryTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ Tests.Core.Node.accumTests
      , edslAgentsTests
      , edslDispatchTests
      , edslRulesTests
      , edslRulesUnitTests
      , allocationCountTests
      , reducerUnitTests
      , memoryManagerUnitTests
      , mealyCoreUnitTests
      , cycleBudgetTests
      , loaderUnitTests
      , symmetryTests
      ]
