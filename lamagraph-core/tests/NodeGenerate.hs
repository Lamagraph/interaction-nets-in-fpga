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

type GenNode (n :: Nat) = forall m. (MonadGen m) => m (Node n)

genNode :: forall m n. (MonadGen m, KnownNat n) => m (Node n)
genNode = do
  port <- genPort
  ports <- genVec genMbPort :: m (Vec n _)
  return $ Node port ports
 where
  genMbPort =
    Gen.frequency
      [ (70, Just <$> genPort)
      , (30, return Nothing)
      ]
