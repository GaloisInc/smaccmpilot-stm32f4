{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.SensorFusion
  ( app
  ) where

import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART
import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import PX4.Tests.Platforms
import PX4.Tests.AllSensors (baro_mag_manager)
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import SMACCMPilot.Hardware.GPS.UBlox
import SMACCMPilot.Hardware.MPU6000
import SMACCMPilot.INS.Tower

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let gps = px4platform_gps px4platform
  (gpsi, _gpso) <- uartTower (px4platform_clockconfig . topx4)
                             (uart_periph gps)
                             (uart_pins gps)
                             38400
                             (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)

  (ms5611meas, hmc5883lsample) <- baro_mag_manager topx4

  let mpu6000 = px4platform_mpu6000 px4platform
  (sreq, sres, sready) <- spiTower (px4platform_clockconfig . topx4)
                                   [mpu6000_spi_device mpu6000]
                                   (mpu6000_spi_pins mpu6000)
  g_sample <- channel
  a_sample <- channel
  mpu6000SensorManager sreq sres sready (fst g_sample) (fst a_sample) (SPIDeviceHandle 0)

  states <- sensorFusion (snd a_sample) (snd g_sample) hmc5883lsample ms5611meas (snd position)

  (_uarti, uartout) <- px4ConsoleTower topx4

  p <- period (Milliseconds 40) -- can't send states much faster than 25Hz at 115200bps

  monitor "sensorsender" $ do
    last_state <- state "last_state"
    handler states "buffer_state" $ callback $ refCopy last_state

    (buf :: Ref Global (Array 88 (Stored Uint8))) <- state "state_ser_buf"
    handler p "send_state" $ do
      e <- emitter uartout (2 * 88 + 3) -- twice buf size plus tag and two fbos
      callback $ const $ noReturn $ do
        packInto buf 0 $ constRef last_state
        let tag = 102 -- 'f' for fusion
        HX.encode tag (constRef buf) (emitV e)

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

