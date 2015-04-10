
module SMACCMPilot.Commsec.Tower
  ( commsecEncodeTower
  , commsecDecodeTower
  , commsecTowerDeps
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Ivory.Module
import SMACCMPilot.Commsec.Ivory.Error
import SMACCMPilot.Commsec.Ivory.Artifacts

commsecEncodeTower :: String
                   -> KeySalt
                   -> ChanOutput PlaintextArray
                   -> Tower p (ChanOutput CyphertextArray)
commsecEncodeTower n ks pt_chan = do
  commsecTowerDeps
  f <- freshname n
  let named t = t ++ "_" ++ showUnique f
      ce = gecEncode ks (named "ctx")
  ct_chan <- channel
  monitor "CommsecEncodeState" $ do
    monitorModuleDef $ gec_encode_moddef ce
    handler systemInit "gec_encode_init" $
      callback $ const $ gec_encode_init ce
    handler pt_chan "plaintext_encode" $ do
      e <- emitter (fst ct_chan) 1
      callback $ \ pt -> do
        ct <- local (iarray [])
        res <- gec_encode_run ce pt ct
        when (res ==? success) $
          emit e (constRef ct)
  return (snd ct_chan)


commsecDecodeTower :: String
                   -> KeySalt
                   -> ChanOutput CyphertextArray
                   -> Tower p (ChanOutput PlaintextArray)
commsecDecodeTower n ks ct_chan = do
  commsecTowerDeps
  f <- freshname n
  let named t = t ++ "_" ++ showUnique f
      cd = gecDecode ks (named "ctx")
  pt_chan <- channel
  monitor "commsecDecodeState" $ do
    monitorModuleDef $ gec_decode_moddef cd
    handler systemInit "gec_decode_init" $
      callback $ const $ gec_decode_init cd
    handler ct_chan "cyphertext_decode" $ do
      e <- emitter (fst pt_chan) 1
      callback $ \ ct -> do
        pt <- local (iarray [])
        res <- gec_decode_run cd ct pt
        when (res ==? success) $
          emit e (constRef pt)
  return (snd pt_chan)

commsecTowerDeps :: Tower p ()
commsecTowerDeps = mapM_ towerArtifact commsecArtifacts
