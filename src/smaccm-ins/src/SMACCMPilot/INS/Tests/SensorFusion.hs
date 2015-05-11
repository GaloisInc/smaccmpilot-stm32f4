{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SMACCMPilot.INS.Tests.SensorFusion
  ( app
  ) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import SMACCMPilot.Hardware.SensorManager
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize
import SMACCMPilot.Hardware.Tests.Ublox (uartUbloxGPSTower)
import SMACCMPilot.Comm.Ivory.Types (typeModules)
import SMACCMPilot.INS.Tower

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let gps = px4platform_gps px4platform
  position <- channel
  uartUbloxGPSTower (px4platform_clockconfig . topx4) gps (fst position)

  (accel_s, gyro_s, mag_s, baro_s) <- sensorManager tosens tocc

  states <- sensorFusion accel_s gyro_s mag_s baro_s (snd position)

  (uartout, _uarti) <- px4ConsoleTower topx4

  p <- period (Milliseconds 40) -- can't send states much faster than 25Hz at 115200bps

  buffered_state <- channel
  monitor "sensorsender" $ do
    last_state <- state "last_state"
    handler states "buffer_state" $ callback $ refCopy last_state

    handler p "deliver_state" $ do
      e <- emitter (fst buffered_state) 1
      callback $ const $ emit e $ constRef last_state

    sampleSender 'f' (Proxy :: Proxy 88) (snd buffered_state) uartout

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerDepends typeModules
  mapM_ towerModule typeModules
  mapM_ towerArtifact serializeArtifacts
  where
  tosens = px4platform_sensors . topx4
  tocc   = px4platform_clockconfig . topx4
