module Core.Loader where

import Clash.Prelude
import Core.Node

-- | Get `Node` by his `Address` from RAM. Actually, preparing to reducer work.
loader ::
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat numberOfPorts
  ) =>
  (Signal dom Address -> Signal dom (Node numberOfPorts)) ->
  Signal dom (Maybe Address) ->
  Signal dom (Maybe (LoadedNode numberOfPorts))
loader ram mbAddressToLoad =
  mkLoadedNode <$> mbNode <*> mbAddressToLoad
 where
  mkLoadedNode node address = LoadedNode <$> node <*> address
  mbNode = case sequenceA mbAddressToLoad of
    Nothing -> pure Nothing
    Just n -> sequenceA $ Just (ram n)
