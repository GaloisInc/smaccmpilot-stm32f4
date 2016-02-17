{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM.Decode
  ( ppmDecodeTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Time
import SMACCMPilot.Hardware.PPM.Decode.Types
import SMACCMPilot.Hardware.PPM.PulseCapture.Types
import qualified SMACCMPilot.Comm.Ivory.Types.RcInput as RC

ppmDecodeTower :: ChanOutput ('Struct "pulse_capture")
               -> ChanInput ('Struct "rc_input")
               -> Tower e ()
ppmDecodeTower pulse_chan out_chan = monitor "ppmDecode" $ do
  s  <- stateInit "ppmState" (ival ppmSync)
  ix <- stateInit "ppmFrameIx" (ival (0 :: Ix 8))
  f  <- stateInit "ppmFrame" (iarray (repeat (ival 0)))
  rcin <- state "ppmRCInput"
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
              ifte_ ((st ==? ppmSync .|| st ==? ppmMark)
                    .&& i >=? (minimumPulses - 1))
                (frameToRCInput (constRef f) rcin
                >> store (rcin ~> RC.valid) true)
                (store (rcin ~> RC.valid) false)
              t <- getTime
              store (rcin ~> RC.time) (timeMicrosFromITime t)
              emit e (constRef rcin)
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

frameToRCInput :: ConstRef s1 ('Array 8 ('Stored Uint16))
               -> Ref      s2 ('Struct "rc_input")
               -> Ivory eff ()
frameToRCInput frame rcin = do
  i0  <- deref (frame ! 0)
  i1  <- deref (frame ! 1)
  i2  <- deref (frame ! 2)
  i3  <- deref (frame ! 3)
  i4  <- deref (frame ! 4)
  i5  <- deref (frame ! 5)
  store (rcin ~> RC.roll)     i0
  store (rcin ~> RC.pitch)    i1
  store (rcin ~> RC.throttle) i2
  store (rcin ~> RC.yaw)      i3
  store (rcin ~> RC.switch1)  i4
  store (rcin ~> RC.switch2)  i5

