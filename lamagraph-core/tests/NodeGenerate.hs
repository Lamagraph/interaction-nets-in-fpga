{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module NodeGenerate where

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Clash.Prelude
import qualified Clash.Sized.Vector as Vec
import Core.Node
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (MonadGen)
import qualified Hedgehog.Range as Range
import qualified Prelude

type UnsignedRange (n :: Nat) = Range.Range (Unsigned n)

genAddress :: (MonadGen m) => m Address
genAddress = genUnsigned (Range.linearBounded :: UnsignedRange 16)

-- | Honest generating uniq addresses. May be very slow.
genUniqAddresses :: (MonadGen m, KnownNat n) => m (Vec n Address)
genUniqAddresses = Gen.filterT addressesAreUniq (genVec genAddress)
 where
  addressesAreUniq vec = allDifferent $ toList vec
  allDifferent list = case list of
    [] -> True
    x : xs -> x `notElem` xs && allDifferent xs

genPort :: (MonadGen m) => m Port
genPort = do
  address <- genAddress
  Port address <$> Gen.bool

genPortVisitedFlag :: (MonadGen m) => Bool -> m Port
genPortVisitedFlag visited = do
  address <- genAddress
  return $ Port address visited

type GenNode (n :: Nat) = forall m. (MonadGen m) => m (Node n)

genNode :: forall m n. (MonadGen m, KnownNat n) => m (Node n)
genNode = do
  port <- genPort
  ports <- genMbObjectsVec genPort :: _ (Vec n _)
  return $ Node port ports

genLoadedNodeByGivenAddresses :: (MonadGen m) => Address -> Address -> [Address] -> m (LoadedNode 5)
genLoadedNodeByGivenAddresses nodeAddr prPortAddr secPortsAddr =
  return $ LoadedNode (Node (Port prPortAddr False) secPorts) nodeAddr
 where
  secPorts = Vec.unsafeFromList (mkPorts secPortsAddr) :: Vec 5 (Maybe Port)
  mkPorts = Prelude.map (\a -> Just (Port a False))

genMbObjectsVec :: forall m n a. (MonadGen m, KnownNat n) => m a -> m (Vec n (Maybe a))
genMbObjectsVec genObjectFunc = genVec genFunc
 where
  genFunc =
    Gen.frequency
      [ (70, Just <$> genObjectFunc)
      , (30, return Nothing)
      ]
