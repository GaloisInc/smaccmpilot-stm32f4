
module SMACCMPilot.Commsec.Tower
  ( commsecEncodeTower
  , commsecEncodeTower'
  , commsecDecodeTower
  , commsecDecodeTower'
  , keyableCommsecEncodeTower
  , keyableCommsecEncodeTower'
  , keyableCommsecDecodeTower
  , keyableCommsecDecodeTower'
  , commsecTowerDeps
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Ivory.Module
import SMACCMPilot.Commsec.Ivory.Error
import SMACCMPilot.Commsec.Ivory.Artifacts

commsecEncodeTower :: String
                   -> Init SymKeySaltArray
                   -> ChanOutput PlaintextArray
                   -> Tower p (ChanOutput CyphertextArray)
commsecEncodeTower n ks_ival pt_chan = do
  ct_chan <- channel
  commsecEncodeTower' n ks_ival pt_chan (fst ct_chan)
  return (snd ct_chan)

commsecEncodeTower' :: String
                   -> Init SymKeySaltArray
                   -> ChanOutput PlaintextArray
                   -> ChanInput CyphertextArray
                   -> Tower p ()
commsecEncodeTower' n ks_ival pt_chan ct_chan = do
  ks_chan <- channel
  monitor "commsecEncodeStaticKey" $ do
    handler systemInit "configureStaticKey" $ do
      e <- emitter (fst ks_chan) 1
      callback $ const $ do
        ks <- local ks_ival
        emit e (constRef ks)
  keyableCommsecEncodeTower' n (snd ks_chan) pt_chan ct_chan

keyableCommsecEncodeTower :: String
                          -> ChanOutput SymKeySaltArray
                          -> ChanOutput PlaintextArray
                          -> Tower p (ChanOutput CyphertextArray)
keyableCommsecEncodeTower n ks_chan pt_chan = do
  ct_chan <- channel
  keyableCommsecEncodeTower' n ks_chan pt_chan (fst ct_chan)
  return (snd ct_chan)

keyableCommsecEncodeTower' :: String
                          -> ChanOutput SymKeySaltArray
                          -> ChanOutput PlaintextArray
                          -> ChanInput CyphertextArray
                          -> Tower p ()
keyableCommsecEncodeTower' n ks_chan pt_chan ct_chan = do
  commsecTowerDeps
  f <- freshname n
  let named t = t ++ "_" ++ showUnique f
      ce = gecEncode (named "ctx")
  monitor "commsecEncodeState" $ do
    validkey <- stateInit "validkey" (ival false)
    monitorModuleDef $ gec_encode_moddef ce
    handler ks_chan "gec_encode_init" $
      callback $ \sk -> do
        gec_encode_init ce sk
        store validkey true
    handler pt_chan "plaintext_encode" $ do
      e <- emitter ct_chan 1
      callback $ \ pt -> do
        vk <- deref validkey
        when vk $ do
          ct <- local (iarray [])
          res <- gec_encode_run ce pt ct
          when (res ==? success) $
            emit e (constRef ct)

commsecDecodeTower :: String
                   -> Init SymKeySaltArray
                   -> ChanOutput CyphertextArray
                   -> Tower p (ChanOutput PlaintextArray)
commsecDecodeTower n ks_ival ct_chan = do
  pt_chan <- channel
  commsecDecodeTower' n ks_ival ct_chan (fst pt_chan)
  return (snd pt_chan)

commsecDecodeTower' :: String
                   -> Init SymKeySaltArray
                   -> ChanOutput CyphertextArray
                   -> ChanInput PlaintextArray
                   -> Tower p ()
commsecDecodeTower' n ks_ival ct_chan pt_chan = do
  ks_chan <- channel
  monitor "commsecDecodeStaticKey" $ do
    handler systemInit "configureStaticKey" $ do
      e <- emitter (fst ks_chan) 1
      callback $ const $ do
        ks <- local ks_ival
        emit e (constRef ks)
  keyableCommsecDecodeTower' n (snd ks_chan) ct_chan pt_chan


keyableCommsecDecodeTower :: String
                          -> ChanOutput SymKeySaltArray
                          -> ChanOutput CyphertextArray
                          -> Tower p (ChanOutput PlaintextArray)
keyableCommsecDecodeTower n ks_chan ct_chan = do
  pt_chan <- channel
  keyableCommsecDecodeTower' n ks_chan ct_chan (fst pt_chan)
  return (snd pt_chan)

keyableCommsecDecodeTower' :: String
                          -> ChanOutput SymKeySaltArray
                          -> ChanOutput CyphertextArray
                          -> ChanInput PlaintextArray
                          -> Tower p ()
keyableCommsecDecodeTower' n ks_chan ct_chan pt_chan = do
  commsecTowerDeps
  f <- freshname n
  let named t = t ++ "_" ++ showUnique f
      cd = gecDecode (named "ctx")
  monitor "commsecDecodeState" $ do
    validkey <- stateInit "validkey" (ival false)
    monitorModuleDef $ gec_decode_moddef cd
    handler ks_chan "gec_decode_init" $
      callback $ \sk -> do
        gec_decode_init cd sk
        store validkey true
    handler ct_chan "cyphertext_decode" $ do
      e <- emitter pt_chan 1
      callback $ \ ct -> do
        vk <- deref validkey
        when vk $ do
          pt <- local (iarray [])
          res <- gec_decode_run cd ct pt
          when (res ==? success) $
            emit e (constRef pt)

commsecTowerDeps :: Tower p ()
commsecTowerDeps = mapM_ towerArtifact commsecArtifacts
