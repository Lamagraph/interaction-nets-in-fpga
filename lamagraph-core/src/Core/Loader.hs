module Core.Loader where

import Clash.Prelude
import Core.MemoryManager.NodeChanges
import Core.Node

-- | Get `Node` by his `AddressNumber` from RAM. Actually, preparing to reducer work.
loader ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat numberOfPorts
  ) =>
  (Signal dom AddressNumber -> Signal dom (Node numberOfPorts)) ->
  Signal dom (Maybe AddressNumber) ->
  Signal dom (Maybe (LoadedNode numberOfPorts))
loader ram mbAddressNumberToLoad =
  mkLoadedNode <$> mbNode <*> mbAddressNumberToLoad
 where
  mkLoadedNode node address = LoadedNode <$> node <*> address
  mbNode = case sequenceA mbAddressNumberToLoad of
    Nothing -> pure Nothing
    Just n -> sequenceA $ Just (ram n)
