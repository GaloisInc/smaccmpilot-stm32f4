{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.Commsec
  ( commsecEncodeDatalink
  , commsecDecodeDatalink
  , padTower
  , unpadTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Datalink.Mode
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Tower

unpadTower :: ChanOutput CyphertextArray -> ChanInput PlaintextArray -> Tower e ()
unpadTower unpad_in unpad_out = do
  monitor "unpad" $ do
    handler unpad_in "unpad_in" $ do
      e <- emitter unpad_out 1
      callback $ \in_buf -> do
        o <- local izero
        arrayCopy o in_buf 0 (fromIntegral plaintextSize)
        emit e (constRef o)

padTower :: ChanOutput PlaintextArray -> ChanInput CyphertextArray -> Tower e ()
padTower pad_in pad_out = do
  monitor "pad" $ do
    handler pad_in "pad_in" $ do
      e <- emitter pad_out 1
      callback $ \in_buf -> do
        o <- local izero
        arrayCopy o in_buf 0 (fromIntegral plaintextSize)
        emit e (constRef o)

commsecEncodeDatalink :: (e -> DatalinkMode)
               -> ChanOutput PlaintextArray
               -> ChanInput CyphertextArray
               -> Tower e ()
commsecEncodeDatalink todm pt ct = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> padTower pt ct
    SymmetricCommsecMode DatalinkServer sk ->
      commsecEncodeTower' "dl" (symKeySaltArrayIval (sk_s2c sk)) pt ct
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxy.datalinkCommsecEncode: "
                  ++ "unsupported datalink mode " ++ show datalinkMode )

commsecDecodeDatalink :: (e -> DatalinkMode)
               -> ChanOutput CyphertextArray
               -> ChanInput PlaintextArray
               -> Tower e ()
commsecDecodeDatalink todm ct pt = do
  datalinkMode <- fmap todm getEnv
  case datalinkMode of
    PlaintextMode -> unpadTower ct pt
    SymmetricCommsecMode DatalinkServer sk ->
      commsecDecodeTower' "dl" (symKeySaltArrayIval (sk_c2s sk)) ct pt
    _ -> error ("SMACCMPilot.Flight.Datalink.CAN.TestProxy.datalinkDecode: "
                  ++ "unsupported datalink mode " ++ show datalinkMode )

