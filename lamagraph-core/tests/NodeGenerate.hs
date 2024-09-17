{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module NodeGenerate where

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector
import Clash.Prelude
import Core.Node
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (MonadGen)
import qualified Hedgehog.Range as Range

type UnsignedRange (n :: Nat) = Range.Range (Unsigned n)

genAddress :: (MonadGen m) => m Address
genAddress = genUnsigned (Range.linearBounded :: UnsignedRange 16)

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

genMbObjectsVec :: forall m n a. (MonadGen m, KnownNat n) => m a -> m (Vec n (Maybe a))
genMbObjectsVec genObjectFunc = genVec genFunc
 where
  genFunc =
    Gen.frequency
      [ (70, Just <$> genObjectFunc)
      , (30, return Nothing)
      ]
