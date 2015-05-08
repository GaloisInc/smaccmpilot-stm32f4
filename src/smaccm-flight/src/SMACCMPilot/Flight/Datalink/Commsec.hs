{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.Commsec
  ( symmetricCommsecDatalink
  , plaintextCommsecDatalink
  , commsecDatalink
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

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
  unpadTower unpad_in = do
    out <- channel
    monitor "unpad" $ do
      handler unpad_in "unpad_in" $ do
        e <- emitter (fst out) 1
        callback $ \in_buf -> do
          o <- local izero
          arrayCopy o in_buf 0 (fromIntegral plaintextSize)
          emit e (constRef o)
    return (snd out)

  padTower :: ChanOutput PlaintextArray -> Tower e (ChanOutput CyphertextArray)
  padTower pad_in = do
    out <- channel
    monitor "pad" $ do
      handler pad_in "pad_in" $ do
        e <- emitter (fst out) 1
        callback $ \in_buf -> do
          o <- local izero
          arrayCopy o in_buf 0 (fromIntegral plaintextSize)
          emit e (constRef o)
    return (snd out)

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
