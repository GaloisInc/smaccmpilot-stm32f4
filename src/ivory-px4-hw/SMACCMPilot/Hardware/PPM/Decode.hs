{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM.Decode
  ( ppmDecodeTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Hardware.PPM.Decode.Types
import SMACCMPilot.Hardware.PPM.PulseCapture.Types

ppmDecodeTower :: ChanOutput (Struct "pulse_capture")
               -> ChanInput (Array 8 (Stored Uint16))
               -> Tower e ()
ppmDecodeTower pulse_chan out_chan = monitor "ppmDecode" $ do
  s  <- stateInit "ppmState" (ival ppmSync)
  ix <- stateInit "ppmFrameIx" (ival (0 :: Ix 8))
  f  <- stateInit "ppmFrame" (iarray (repeat (ival 0)))
  handler pulse_chan "pluse_handler" $ do
    e <- emitter out_chan 1
    callback $ \p -> do
      w  <- deref (p ~> width)
      m  <- deref (p ~> missed)
      st <- deref s
      i  <- deref ix
      cond_
        [ m                 ==> store s ppmSync
        , (w >? syncWidth) ==> do
            when (w >? syncWidth) $ do
              store ix 0
              store s ppmMark
              when ((st ==? ppmSync .|| st ==? ppmMark)
                    .&& i >=? (minimumPulses - 1)) $ do
                emit e (constRef f)
              arrayMap (\x -> store (f ! x) 0)
        , (st ==? ppmMark)  ==> do
            ifte_ (w >? markMinWidth .&& w <? markMaxWidth)
              (do store (f ! i) w
                  store s ppmSpace)
              (store s ppmSync)
        , (st ==? ppmSpace) ==> do
            markw <- deref (f ! i)
            let pw = w + markw
            ifte_ (pw >? pulseMinWidth .&& pw <? pulseMaxWidth)
              (do store (f ! i) (w + markw)
                  ifte_ (i ==? 7)
                    (do store s ppmSync)
                    (do store s ppmMark
                        store ix (i + 1)))
              (do store (f ! i) 0
                  store s ppmSync)
        ]
  where
  syncWidth = 3500
  markMinWidth = 200
  markMaxWidth = 500
  pulseMinWidth = 900
  pulseMaxWidth = 2100
  minimumPulses = 6
