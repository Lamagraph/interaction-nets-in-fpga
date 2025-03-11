{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Tests.Core.Unit.MemoryManager.MemoryManager where

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Core.Concrete.Initial
import Core.MemoryManager.MemoryManager
import Core.Node
import Prelude

idApplyToIdTwoFreeAddresses :: TestTree
idApplyToIdTwoFreeAddresses =
  testCase "give two free addresses: first three are busy" $
    assertBool
      ("expected:\n" ++ show expectedAddresses ++ "\nactual:\n" ++ show (Prelude.head (C.sampleN 1 systemActualAddresses)))
      addressesAreEqual
 where
  expectedAddresses = Just 4 C.:> Just 3 C.:> C.Nil :: C.Vec 2 (Maybe AddressNumber)
  (systemActualAddresses, _) = giveAddresses @(2 C.^ C.BitSize AddressNumber) @2 (pure 2) (pure initialIdApplyToIdMM)
  addressesAreEqual = or (C.sampleN @C.System 1 (systemActualAddresses C..==. pure expectedAddresses))

idApplyToIdGiveActiveAddress :: TestTree
idApplyToIdGiveActiveAddress =
  testCase "get address of active node" $
    assertBool
      ("expected:\n" ++ show expectedAddress ++ "\nactual:\n" ++ show (Prelude.head (C.sampleN 1 systemActualAddress)))
      addressesAreEqual
 where
  expectedAddress = Just (0 :: AddressNumber)
  systemActualAddress = giveActiveAddressNumber (pure initialIdApplyToIdMM)
  addressesAreEqual = or (C.sampleN @C.System 1 (systemActualAddress C..==. pure expectedAddress))

memoryManagerUnitTests :: TestTree
memoryManagerUnitTests =
  testGroup
    "MemoryManager"
    [ idApplyToIdTwoFreeAddresses
    , idApplyToIdGiveActiveAddress
    ]
