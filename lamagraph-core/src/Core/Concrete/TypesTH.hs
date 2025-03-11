module Core.Concrete.TypesTH where

import Language.Haskell.TH
import Prelude

makeAgentsType :: String -> [String] -> Maybe Int -> Q [Dec]
makeAgentsType agentTypeName agentsName maybeCountOfInts = pure [DataD [] (mkName agentTypeName) [] Nothing (agents ++ ints) []]
 where
  ints =
    maybe
      []
      (\countOfInts -> map (\number -> NormalC (mkName $ "UnsignedInt" ++ show number) []) [1 .. countOfInts])
      maybeCountOfInts
  agents = map (\n -> NormalC (mkName n) []) agentsName
