{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.Commsec
  ( symmetricCommsecDatalink
  , plaintextCommsecDatalink
  , commsecDatalink
  ) where

import Ivory.Tower

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Tower

symmetricCommsecDatalink  :: SymmetricKey
                          -> ( ChanOutput PlaintextArray
                              -> Tower e (a, ChanOutput PlaintextArray))
                          -> ChanOutput CyphertextArray
                          -> Tower e (a, ChanOutput CyphertextArray)
symmetricCommsecDatalink sk k ct_in = do
  pt_in <- commsecDecodeTower "frame" (symKeySaltArrayIval (sk_c2s sk)) ct_in
  (a, pt_out) <- k pt_in
  ct_out <- commsecEncodeTower "frame" (symKeySaltArrayIval (sk_s2c sk)) pt_out
  return (a, ct_out)

plaintextCommsecDatalink  :: ( ChanOutput PlaintextArray
                             -> Tower e (a, ChanOutput PlaintextArray))
                          -> ChanOutput CyphertextArray
                          -> Tower e (a, ChanOutput CyphertextArray)
plaintextCommsecDatalink k pt_in = do
  unpadded_pt_in <- unpadTower pt_in
  (a, unpadded_pt_out) <- k unpadded_pt_in
  pt_out <- padTower unpadded_pt_out
  return (a, pt_out)
  where
  unpadTower :: ChanOutput CyphertextArray -> Tower e (ChanOutput PlaintextArray)
  unpadTower = undefined -- XXX FIXME
  padTower :: ChanOutput PlaintextArray -> Tower e (ChanOutput CyphertextArray)
  padTower = undefined -- XXX FIXME

commsecDatalink :: (e -> DatalinkMode)
                -> ( ChanOutput PlaintextArray
                    -> Tower e (a, ChanOutput PlaintextArray))
                -> ChanOutput CyphertextArray
                -> Tower e (a, ChanOutput CyphertextArray)
commsecDatalink todm k ctin = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> plaintextCommsecDatalink k ctin
    SymmetricCommsecMode sk -> symmetricCommsecDatalink sk k ctin
