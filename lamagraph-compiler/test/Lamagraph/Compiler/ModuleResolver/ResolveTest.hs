module Lamagraph.Compiler.ModuleResolver.ResolveTest (resolverUnitTests) where

import Relude hiding (show)

import Prelude (show)
import Control.Exception (try)
import Test.Tasty
import Test.Tasty.HUnit

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Resolve.Module
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Syntax

-- | Resolves a module. The resolver is deeply lazy: a failing resolution
-- yields a value whose fields raise 'ModuleResolverError' only when forced.
-- We fully force the result (via its 'Show' rendering) inside the 'try' so
-- the error surfaces here instead of escaping later as a poisoned thunk.
resolveOrFail :: LmlModule LmlcPs -> IO (Either ModuleResolverError (LmlModule LmlcMr))
resolveOrFail modPs = try $ do
  let modMr = resolveModuleDefEnv modPs
  forceShow modMr
  return modMr
 where
  forceShow :: Show a => a -> IO ()
  forceShow x = length (show x) `seq` return ()

-- | Parses a source and expects the resolver to throw the matching error.
expectResolverError :: String -> String -> (ModuleResolverError -> Bool) -> TestTree
expectResolverError testName src isError = testCase testName $
  case parseLamagraphML (fromString src) of
    Left parseErr -> assertFailure ("parse error: " <> parseErr)
    Right modPs -> do
      result <- resolveOrFail modPs
      case result of
        Left err
          | isError err -> return ()
          | otherwise -> assertFailure (show err)
        Right _ -> assertFailure "expected a resolver error"

longidentEq :: Longident -> Text -> Bool
longidentEq (Longident name) expected = toList name == [expected]

resolverUnitTests :: TestTree
resolverUnitTests =
  testGroup
    "Resolver unit tests"
    [ expectResolverError
        "NameNotFound on unbound variable"
        "module M\n\nlet x = y\n"
        (\case
          NameNotFound name -> longidentEq name "y"
          _ -> False
        )
    , expectResolverError
        "ModuleNotFound on unknown open"
        "module M\n\nopen NoSuch\n"
        (\case
          ModuleNotFound name -> longidentEq name "NoSuch"
          _ -> False
        )
    , expectResolverError
        "ConstructorNotFound on unknown constructor"
        "module M\n\nlet x = NoSuch 1\n"
        (\case
          ConstructorNotFound name -> longidentEq name "NoSuch"
          _ -> False
        )
    ]
