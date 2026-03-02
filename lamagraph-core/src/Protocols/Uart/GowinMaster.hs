{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Protocols.Uart.GowinMaster where

import Clash.Annotations.Primitive
import Clash.Prelude
import Control.Monad.State.Strict
import Core.Node
import Data.Maybe (fromJust, isJust)
import Data.String.Interpolate (__i)
import Protocols.Uart.Helper

gowinMasterUARTIp ::
  (KnownDomain dom) =>
  Clock dom -> -- I_CLK
  Reset dom -> -- I_RESETN
  Enable dom -> -- I_TX_EN
  Signal dom (BitVector 3) -> -- I_WADDR[2:0]
  Enable dom -> -- I_RX_EN
  Signal dom (BitVector 3) -> -- I_RADDR[2:0]
  Signal dom Bit -> -- SIN
  Signal dom (BitVector 8) -> -- I_WDATA[7:0]
  -- Signal dom Bit -> -- DCDn
  -- Signal dom Bit -> -- CTSn
  -- Signal dom Bit -> -- DSRn
  -- Signal dom Bit -> -- Rin
  ( Signal dom (BitVector 8) -- O_RDATA[7:0]
  , Signal dom Bit -- SOUT
  , Signal dom Bit -- RxRDYn
  , Signal dom Bit -- TxRDYn
  -- , Signal dom Bit -- DDIS
  -- , Signal dom Bit -- INTR
  -- , Signal dom Bit -- DTRn
  -- , Signal dom Bit -- RTSn
  )
gowinMasterUARTIp !_ !_ !_ !_ !_ !_ !_ !_ = def
-- {-# NOINLINE gowinMasterUARTIp #-}
{-# ANN gowinMasterUARTIp hasBlackBox #-}
{-# OPAQUE gowinMasterUARTIp #-}
{-# ANN
  gowinMasterUARTIp
  ( InlineYamlPrimitive
      [Verilog, SystemVerilog]
      [__i|
          BlackBox:
            kind: Declaration
            name : Protocols.Uart.GowinMaster.gowinMasterUARTIp
            type: |-
              gowinMasterUARTIp ::
                (KnownDomain dom) =>
                  Clock dom -> -- I_CLK
                  Reset dom -> -- I_RESETN
                  Signal dom Bit -> -- I_TX_EN
                  Signal dom (BitVector 3) -> -- I_WADDR[2:0]
                  Signal dom Bit -> -- I_RX_EN
                  Signal dom (BitVector 3) -> -- I_RADDR[2:0]
                  Signal dom Bit -> -- SIN
                  Signal dom (BitVector 8) -> -- I_WDATA[7:0]
                  ( Signal dom (BitVector 8) -- O_RDATA[7:0]
                  , Signal dom Bit -- SOUT
                  )
            template: |-
              // gowinMasterUARTIp begin

              UART_MASTER_Top ~GENSYM[master_uart_ip][0] (
                      .I_CLK    ( ~ARG[1]    ),
                      .I_RESETN ( ~ARG[2]    ),
                      .I_TX_EN  ( ~ARG[3]    ),
                      .I_WADDR  ( ~ARG[4]    ),
                      .I_WDATA  ( ~ARG[8]    ),
                      .I_RX_EN  ( ~ARG[5]    ),
                      .I_RADDR  ( ~ARG[6]    ),
                      .O_RDATA  ( ~RESULT[10:3] ),
                    // Processor interface
                      .DDIS     (            ), // Driver disable
                      .INTR     (            ), // Interrupt
                    // Receiver interface
                      .SIN      ( ~ARG[7]    ), // Receiver serial input
                      .RxRDYn   ( ~RESULT[1] ), // Receiver ready
                    // Transmitter interface
                      .SOUT     ( ~RESULT[2] ), // Transmitter serial output
                      .TxRDYn   ( ~RESULT[0] ), // Transmitter ready
                    // Modem interface
                      .DCDn     (    1'b0    ), // Data Carrier Detect
                      .CTSn     (    1'b0    ), // Clear To Send
                      .DSRn     (    1'b0    ), // Data Set Ready
                      .RIn      (    1'b0    ), // Ring Indicator
                      .DTRn     (            ), // Data Terminal Ready
                      .RTSn     (            ) // Request To Send
                    );
              // gowinMasterUARTIp end

      |]
  )
  #-}

transmitByUARTState :: (Byte, BitVector 8) -> UARTState
transmitByUARTState (d, rdata) = do
  (status, r) <- get
  let
    defaultSt = (r, False, False, False)
    configLCRHandler i = case i of
      0 -> do
        put (ConfigLCR 1, configLCR)
        pure (configLCR, True, False, False)
      1 -> do
        put (ConfigLCR 2, r)
        pure defaultSt
      2 -> do
        put (ReadStatus 0, r)
        pure defaultSt
      _ -> undefined
    readStatusHandler i = case i of
      0 -> do
        put (ReadStatus 1, configLSR)
        pure (configLSR, False, True, False)
      1 -> do
        put (ReadStatus 2, r)
        pure defaultSt
      2 -> do
        put (ReadStatus 3, r)
        pure defaultSt
      3 -> do
        put (ReadStatus 4, LSR (unpack $ reverseBV $ truncateB rdata))
        pure defaultSt
      4 ->
        if transmittingIsEnable r
          then do
            put (WriteData 0, r)
            pure defaultSt
          else do
            put (ReadStatus 0, r)
            pure defaultSt
      _ -> undefined
    writeDataHandler i = case i of
      0 -> do
        put (WriteData 1, configTHRToWrite d)
        pure (configTHRToWrite d, True, False, True)
      1 -> do
        put (WriteData 2, r)
        pure defaultSt
      2 -> do
        put (ReadStatus 0, r)
        pure defaultSt
      _ -> undefined
  case status of
    ConfigLCR i -> configLCRHandler i
    ReadStatus i -> readStatusHandler i
    WriteData i -> writeDataHandler i

transmitByteByUART ::
  forall dom.
  (KnownDomain dom, HiddenClockResetEnable dom) =>
  Signal dom Byte ->
  (Signal dom Bit, Enable dom)
transmitByteByUART d = (sout, toEnable enBool)
 where
  transmitter =
    mealyS
      transmitByUARTState
      (ConfigLCR 0, RBR (RBRTable 0))
  (uartReg, tx_en_bool, rx_en_bool, enBool) =
    unbundle $
      transmitter
        (bundle (d, rdata))
  rx_en = toEnable rx_en_bool
  tx_en = toEnable tx_en_bool
  (waddr, wdata) = unbundle (encodeUARTRegister <$> uartReg)
  raddr = fst . encodeUARTRegister <$> uartReg
  (rdata, sout, _, _) = (hideReset $ hideClock gowinMasterUARTIp) tx_en waddr rx_en raddr def wdata

transmitNodeByUart ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat portsNumber
  , 1 <= portsNumber
  , BitPack agentType
  ) =>
  Signal dom (Maybe (Node portsNumber agentType)) ->
  (Signal dom Bit, Enable dom)
transmitNodeByUart maybeNode = (sout, toEnable isMax)
 where
  (byte, isMax) = exposeEnable (iterateOverDataBool (toBytes . fromJust <$> maybeNode)) rdyToRx
  (sout, rdyToRx) = (isJust <$> maybeNode) `andEnable` transmitByteByUART byte
