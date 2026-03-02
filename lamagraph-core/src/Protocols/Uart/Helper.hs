{- HLINT ignore "Use newtype instead of data" -}
module Protocols.Uart.Helper where

import Clash.Prelude
import Control.Monad.State.Strict

type TX_EN = Bool
type RX_EN = Bool
type Byte = BitVector 8

data RBRTable = RBRTable {rbr :: BitVector 8} deriving (Generic, BitPack, Default, NFDataX)

data THRTable = THRTable {thr :: BitVector 8} deriving (Generic, BitPack, Default, NFDataX)
data IERTable = IERTable
  { rbri :: Bit
  , thri :: Bit
  , rlsi :: Bit
  , msi :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)

data IIRTable = IIRTable
  { intStat :: Bit
  , int01 :: BitVector 2
  , int2 :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)
data LCRTable = LCRTable
  { wls :: BitVector 2
  , stb :: Bit
  , pen :: Bit
  , eps :: Bit
  , sp :: Bit
  , sb :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)
data MCRTable = MCRTable
  { dtr :: Bit
  , rts :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)
data LSRTable = LSRTable
  { rxrdy :: Bit
  , oe :: Bit
  , pe :: Bit
  , fe :: Bit
  , bi :: Bit
  , thre :: Bit
  , tempt :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)

data MSRTable = MSRTable
  { dcts :: Bit
  , ddsr :: Bit
  , teri :: Bit
  , ddcd :: Bit
  , cts :: Bit
  , dsr :: Bit
  , ri :: Bit
  , dcd :: Bit
  }
  deriving (Generic, BitPack, Default, NFDataX)

data MasterUARTRegister
  = RBR RBRTable
  | THR THRTable
  | IER IERTable
  | IIR IIRTable
  | LCR LCRTable
  | MCR MCRTable
  | LSR LSRTable
  | MSR MSRTable
  deriving (Generic, BitPack, NFDataX)

reverseBV :: (KnownNat n) => BitVector n -> BitVector n
reverseBV bv = v2bv $ reverse $ bv2v bv

encodeUARTRegister :: MasterUARTRegister -> (BitVector 3, BitVector 8)
encodeUARTRegister r = case r of
  RBR t -> (0b000, reverseBV $ pack t)
  THR t -> (0b000, reverseBV $ pack t)
  IER t -> (0b001, zeroExtend $ reverseBV $ pack t)
  IIR t -> (0b010, zeroExtend $ reverseBV $ pack t)
  LCR t -> (0b011, zeroExtend $ reverseBV $ pack t)
  MCR t -> (0b100, zeroExtend $ reverseBV $ pack t)
  LSR t -> (0b101, zeroExtend $ reverseBV $ pack t)
  MSR t -> (0b110, reverseBV $ pack t)

data UARTStatus = ConfigLCR (Index 3) | ReadStatus (Index 5) | WriteData (Index 3) deriving (Generic, NFDataX)
type UARTState =
  State
    ( UARTStatus
    , MasterUARTRegister -- read register data (I_RADDR, O_RDATA) or previous state (it should be obvious by context)
    )
    ( MasterUARTRegister
    , TX_EN
    , RX_EN
    , Bool
    )

configLCR :: MasterUARTRegister
configLCR =
  LCR
    ( LCRTable
        { wls = 0b11 -- 8 bit
        , stb = 0b0 -- 1 stop bit
        , pen = 0b0 -- disable parity bit
        , eps = 0b0 -- odd parity (disabled because of `pen`)
        , sp = 0b0 -- disable force parity
        , sb = 0b0 -- disable interrupt
        }
    )

configLSR :: MasterUARTRegister
configLSR = LSR def

transmittingIsEnable :: MasterUARTRegister -> Bool
transmittingIsEnable (LSR (LSRTable{..})) = bitToBool tempt
transmittingIsEnable _ = error "transmittingIsEnable get wrong argument"
transmittingIsEnableTable :: LSRTable -> Bool
transmittingIsEnableTable (LSRTable{..}) = bitToBool (tempt .&. thre)

transmittingIsCompleted :: MasterUARTRegister -> Bool
transmittingIsCompleted (LSR (LSRTable{..})) = bitToBool rxrdy
transmittingIsCompleted _ = error "transmittingIsCompleted get wrong argument"

configTHRToWrite :: Byte -> MasterUARTRegister
configTHRToWrite d = THR (THRTable $ reverseBV d)

toBytes :: (BitPack a) => a -> Vec ((BitSize a + 7) `Div` 8) Byte
toBytes x = bytes
 where
  bits = pack x
  bytes = map v2bv (unconcat d8 (bv2v $ resize bits))

fromBytes :: (BitPack a) => Vec ((BitSize a + 7) `Div` 8) Byte -> a
fromBytes bytes = unpack (resize bits)
 where
  bits = v2bv (concatMap bv2v bytes)

iterateOverDataBool ::
  forall dom n d.
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Vec n d) ->
  (Signal dom d, Signal dom Bool)
iterateOverDataBool bytes = ((!!) <$> bytes <*> idx, isMax)
 where
  idx = register 0 nextIdx
  isMax = idx .==. pure (maxBound :: Index n)
  nextIdx = mux isMax def ((+ 1) <$> idx)
