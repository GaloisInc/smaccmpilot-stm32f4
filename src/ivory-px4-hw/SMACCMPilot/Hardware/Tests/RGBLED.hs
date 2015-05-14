{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.Tests.RGBLED
  ( app
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import SMACCMPilot.Hardware.RGBLED
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize
import SMACCMPilot.Comm.Ivory.Types.RgbLedSetting

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  setting <- channel
  case px4platform_rgbled px4platform of
    Nothing -> error "put an error here"
    Just RGBLED_I2C{..} -> do
      (req, ready) <- i2cTower tocc
                               rgbled_i2c_periph
                               rgbled_i2c_pins
      rgbLedManager req ready (snd setting) rgbled_i2c_addr

  p <- period (Milliseconds 333)
  monitor "changecolor" $ do
    phase <- stateInit "phase" (ival (0 :: Uint8))
    handler p "per" $ do
      e <- emitter (fst setting) 1
      callback $ const $ do
        ph <- deref phase
        cond_
          [ ph ==? 0 ==> do
              v <- local $ istruct
                [ red .= ival 15, blue .= ival 0, green .= ival 0 ]
              emit e (constRef v)
              store phase 1
          , ph ==? 1 ==> do
              v <- local $ istruct
                [ red .= ival 0, blue .= ival 0, green .= ival 15 ]
              emit e (constRef v)
              store phase 2
          , ph ==? 2 ==> do
              v <- local $ istruct
                [ red .= ival 0, blue .= ival 15, green .= ival 0 ]
              emit e (constRef v)
              store phase 0
          ]
  serializeTowerDeps
  where
  tocc = px4platform_clockconfig . topx4
