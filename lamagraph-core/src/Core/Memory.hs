module Core.Memory (
  MemReq (..),
  MemResp (..),
  ToMem (..),
  FromMem (..),
  blockRamMemory,
  DumpState (..),
  dumpStep,
  dumpMemory,
) where

import Clash.Prelude

data MemReq addr cell = MemRead addr | MemWrite addr cell
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX, ShowX)

data MemResp cell = ReadData cell | WriteAck
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX, ShowX)

newtype ToMem addr cell = ToMem {req :: Maybe (MemReq addr cell)}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX, ShowX)

data FromMem cell = FromMem {ready :: Bool, resp :: Maybe (MemResp cell)}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX, ShowX)

blockRamMemory ::
  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX cell) =>
  Vec n cell ->
  Signal dom (ToMem (Index n) cell) ->
  Signal dom (FromMem cell)
blockRamMemory initial input = makeResponse <$> resetting <*> pending <*> ramData
 where
  resetting = unsafeToActiveHigh hasReset
  pending = register Nothing (requestTag <$> accepted)
  accepted = accept <$> resetting <*> pending <*> input
  accept resetActive tag (ToMem request) = case (resetActive, tag) of
    (False, Nothing) -> request
    _ -> Nothing
  requestTag = \case
    Just (MemRead _) -> Just True
    Just (MemWrite _ _) -> Just False
    Nothing -> Nothing
  readAddress = \case
    Just (MemRead address) -> address
    _ -> 0
  writeData = \case
    Just (MemWrite address cell) -> Just (address, cell)
    _ -> Nothing
  ramData = blockRam initial (readAddress <$> accepted) (writeData <$> accepted)
  makeResponse resetActive tag cell
    | resetActive = FromMem False Nothing
    | otherwise = case tag of
        Nothing -> FromMem True Nothing
        Just True -> FromMem False (Just (ReadData cell))
        Just False -> FromMem False (Just WriteAck)

data DumpState n cell
  = DumpIdle
  | DumpRequest (Index n)
  | DumpWait (Index n)
  | DumpSend (Index n) cell
  | DumpFinished
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX, ShowX)

dumpStep ::
  (KnownNat n, 1 <= n) =>
  DumpState n cell ->
  (Bool, Bool, FromMem (Maybe cell)) ->
  (DumpState n cell, (ToMem (Index n) (Maybe cell), Maybe cell))
dumpStep state (start, consumed, memory) = case state of
  DumpIdle -> (if start then DumpRequest 0 else DumpIdle, quiet)
  DumpRequest address ->
    ( if ready memory then DumpWait address else state
    , (ToMem (Just (MemRead address)), Nothing)
    )
  DumpWait address -> case resp memory of
    Just (ReadData Nothing) -> (advance address, quiet)
    Just (ReadData (Just cell)) -> (DumpSend address cell, quiet)
    _ -> (state, quiet)
  DumpSend address cell ->
    (if consumed then advance address else state, (ToMem Nothing, Just cell))
  DumpFinished -> (DumpFinished, quiet)
 where
  quiet = (ToMem Nothing, Nothing)
  advance address
    | address == maxBound = DumpFinished
    | otherwise = DumpRequest (address + 1)

dumpMemory ::
  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX cell) =>
  Signal dom Bool ->
  Signal dom Bool ->
  Signal dom (FromMem (Maybe cell)) ->
  (Signal dom (ToMem (Index n) (Maybe cell)), Signal dom (Maybe cell))
dumpMemory start consumed memory =
  unbundle $ mealy dumpStep DumpIdle (bundle (start, consumed, memory))
