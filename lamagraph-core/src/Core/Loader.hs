module Core.Loader where

import Clash.Prelude
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

{- | Update RAM function. In fact only way to update data in registers.
Change `Node` by given `AddressNumber` at given `Node`
-}
updateRam ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat numberOfPorts
  ) =>
  (Signal dom AddressNumber -> Signal dom (Node numberOfPorts)) ->
  Signal dom AddressNumber ->
  Signal dom (Node numberOfPorts) ->
  (Signal dom AddressNumber -> Signal dom (Node numberOfPorts))
updateRam oldRam newAddressNumber newNode address = mux addressesIsEq newNode oldNode
 where
  addressesIsEq = newAddressNumber .==. address
  oldNode = oldRam address
