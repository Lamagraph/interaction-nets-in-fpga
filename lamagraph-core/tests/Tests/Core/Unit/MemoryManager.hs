{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{- | Unit tests: occupancy bookkeeping ("Core.MemoryManager.MemoryManager").

Fixed fixture `initialIdApplyToIdMM`: cells 0-2 busy, so the first free
addresses handed out are 4 and 3.
-}
module Tests.Core.Unit.MemoryManager where

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Core.Concrete.Initial
import Core.MemoryManager.MemoryManager
import Core.Node
import Tests.Core.Unit.Common.Assert (assertEqualShow)
import Prelude

idApplyToIdTwoFreeAddresses :: TestTree
idApplyToIdTwoFreeAddresses =
  testCase "give two free addresses: first three are busy" $
    assertEqualShow expectedAddresses actualAddresses
 where
  expectedAddresses = Just 4 C.:> Just 3 C.:> C.Nil :: C.Vec 2 (Maybe AddressNumber)
  (actualAddresses, _) = giveAddresses @2 2 initialIdApplyToIdMM

idApplyToIdGiveActiveAddress :: TestTree
idApplyToIdGiveActiveAddress =
  testCase "get address of active node" $
    assertEqualShow expectedAddress systemActualAddress
 where
  expectedAddress = Just (0 :: AddressNumber)
  systemActualAddress = giveActiveAddressNumber initialIdApplyToIdMM

memoryManagerUnitTests :: TestTree
memoryManagerUnitTests =
  testGroup
    "MemoryManager"
    [ idApplyToIdTwoFreeAddresses
    , idApplyToIdGiveActiveAddress
    ]
