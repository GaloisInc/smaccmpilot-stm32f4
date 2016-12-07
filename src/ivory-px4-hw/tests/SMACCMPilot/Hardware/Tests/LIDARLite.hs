{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module SMACCMPilot.Hardware.Tests.LIDARLite (app) where

import Ivory.Language

import Ivory.Tower
import Ivory.Tower.HAL.RingBuffer

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Peripheral.I2C

import qualified Ivory.BSP.STM32F427.GPIO as F427
import qualified Ivory.BSP.STM32F427.I2C as F427
import SMACCMPilot.Hardware.LIDARLite

import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Serialize

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
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

{-
  case px4platform_lidarlite px4platform of
    Nothing -> error "no LIDAR-Lite configured"
    Just LIDARLite{..} -> lidarlite_app topx4 lidarlite_i2c_addr (fst measurements)
-}
  lidarlite_app topx4 (I2CDeviceAddr 0x62) (fst measurements)
  (uarto, _uarti, mon) <- px4ConsoleTower topx4
  monitor "uart" mon
  monitor "lidarliteSender" $ do
    lidarliteSender (snd measurements_buf) uarto

  serializeTowerDeps

lidarlite_app :: (e -> PX4Platform)
               -> I2CDeviceAddr
               -> ChanInput ('Struct "lidarlite_sample")
               -> Tower e ()
lidarlite_app topx4 addr meas = do
  (req, ready) <- i2cTower (px4platform_clockconfig . topx4)
                           F427.i2c1
                           (I2CPins { i2cpins_sda = F427.pinB9
                                    , i2cpins_scl = F427.pinB8 })
  sensors_ready <- px4platform_sensorenable_tower topx4 ready
  lidarliteSensorManager req sensors_ready meas addr
