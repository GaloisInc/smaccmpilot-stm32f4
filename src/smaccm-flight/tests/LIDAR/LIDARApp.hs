{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module LIDAR.LIDARApp (app) where

import Prelude ()
import Prelude.Compat

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.HAL.RingBuffer

import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.Sensors.LIDARLite

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
  -- 18 bytes per sample = 640 samples per second
  -- 640 samples per second = 1.5ms per frame, but we can be conservative
                                       (Milliseconds 10)
  -- buffer depth of 4: we will never use more than 2, and it will fit 3.
                                       (Proxy :: Proxy 4)
                                       (fst measurements_buf)

  case fp_lidarlite fp of
    Nothing -> error "no LIDAR-Lite configured"
    Just ll ->
      lidarlite_app topx4 ll (fst measurements)

  (uarto, _uarti, mon) <- px4ConsoleTower topx4
  monitor "uart" mon
  monitor "lidarliteSender" $ do
    lidarliteSender (snd measurements_buf) uarto

  serializeTowerDeps

lidarlite_app :: (e -> PX4Platform)
              -> LIDARLite
              -> ChanInput ('Struct "lidarlite_sample")
              -> Tower e ()
lidarlite_app topx4 ll meas = do
  px4 <- topx4 <$> getEnv
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                           (fmu24sens_ext_i2c_periph (px4platform_sensors px4))
                           (fmu24sens_ext_i2c_pins   (px4platform_sensors px4))
  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  lidarliteSensorManager req sensors_ready meas ll
