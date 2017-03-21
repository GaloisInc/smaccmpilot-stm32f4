{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module HMC5883L.HMC5883LApp (app) where

import Prelude ()
import Prelude.Compat

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.HAL.RingBuffer

import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Flight.Platform

import SMACCMPilot.Hardware.HMC5883L
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Hardware.Serialize

app :: (e -> FlightPlatform) -> (e -> PX4Platform) -> Tower e ()
app tofp topx4 = do
  fp <- tofp <$> getEnv
  measurements <- channel
  measurements_buf <- channel
  bufferChans (snd measurements)
  -- Buffering timing analysis:
  -- Worst case: 115200 baud
  -- 10 bits per byte (UART framing) = 11520 bytes per second
  -- 23 bytes per sample = 500 samples per second
  -- 500 samples per second = 2ms per frame, but we can be conservative
                                       (Milliseconds 20)
  -- buffer depth of 4: we will never use more than 2, and it will fit 3.
                                       (Proxy :: Proxy 4)
                                       (fst measurements_buf)

  case fp_ext_hmc5883l fp of
    Nothing -> error "no external mag configured"
    Just mag ->
      hmc5883l_app topx4 mag (fst measurements)

  (uarto, _uarti, mon) <- px4ConsoleTower topx4
  monitor "uart" mon
  monitor "hmc5883lSender" $ do
    magSender (snd measurements_buf) uarto

  serializeTowerDeps

hmc5883l_app :: (e -> PX4Platform)
              -> HMC5883L
              -> ChanInput ('Struct "magnetometer_sample")
              -> Tower e ()
hmc5883l_app topx4 HMC5883L{..} meas = do
  px4 <- topx4 <$> getEnv
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                           (fmu24sens_ext_i2c_periph (px4platform_sensors px4))
                           (fmu24sens_ext_i2c_pins   (px4platform_sensors px4))
  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  hmc5883lSensorManager hmc5883l_mag_cal req sensors_ready meas hmc5883l_i2c_addr
