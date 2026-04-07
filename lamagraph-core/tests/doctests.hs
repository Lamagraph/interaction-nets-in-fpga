module Main where

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Prelude

main :: IO ()
main = mainFromCabal "lamagraph-core" =<< getArgs
