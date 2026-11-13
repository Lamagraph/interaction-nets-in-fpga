{-# LANGUAGE TemplateHaskell #-}

{- | Tests for the raw net operations of "Core.Node".

The Hedgehog properties that used to live here (generators in the removed
@NodeGenerate@ module, checks of @isPortToLoad@ \/ @isActive@ \/
@selectAddressToLoad@ \/ @markAllInnerEdges@) were commented out against an
older `Node` API and have been dropped; `accumTests` is the tasty-th-generated
group and is currently empty.
-}
module Tests.Core.Node (accumTests) where

import Test.Tasty
import Test.Tasty.TH

accumTests :: TestTree
accumTests = $(testGroupGenerator)
