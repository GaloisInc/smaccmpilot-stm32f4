{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Light
  ( lightTower
  ) where

import           Ivory.Tower
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Hardware.RGBLED
import           Ivory.BSP.STM32.Driver.I2C

lightTower :: (e -> FlightPlatform)
           -> ControllableVehicleAttrs Attr
           -> Tower e ()
lightTower tofp attrs = do
  -- Invariant: we're only using this RGBLED on a platform where it is the
  -- only device on the given I2C periph!
  fp <- fmap tofp getEnv
  case fp_rgbled fp of
    Nothing -> return ()
    Just RGBLED_I2C{..} -> do
      (req, ready) <- i2cTower tocc
                               rgbled_i2c_periph
                               rgbled_i2c_pins
      rgbLedManager req ready attr_chan rgbled_i2c_addr
  where
  tocc = fp_clockconfig . tofp
  attr_chan = attrReaderChan (rgbLed attrs)


