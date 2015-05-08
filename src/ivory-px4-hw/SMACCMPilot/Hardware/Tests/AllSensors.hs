{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.Tests.AllSensors
  ( app
  ) where

import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.CAN.Sched
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.CAN

import SMACCMPilot.Hardware.CAN
import SMACCMPilot.Hardware.CANMessages

import SMACCMPilot.Hardware.SensorManager
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize
import SMACCMPilot.Hardware.Tests.Ublox (uartUbloxGPSTower)

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let tocc = px4platform_clockconfig . topx4
      tosens = px4platform_sensors . topx4

  let gps = px4platform_gps px4platform
  position <- channel
  uartUbloxGPSTower tocc gps (fst position)

  (accel_meas, gyro_meas, mag_meas, baro_meas) <- sensorManager tosens tocc

  (uartout, _uarti) <- px4ConsoleTower topx4

  div_accel_meas <- rateDivider 4 accel_meas
  div_gyro_meas <- rateDivider 4 gyro_meas

  uartTasks <- sequence
    [ do
        (t, req) <- task name
        monitor name $ f req
        return t
    | (name, f) <-
      [ ("mag", magSender mag_meas)
      , ("baro", baroSender baro_meas)
      , ("gyro", gyroSender div_gyro_meas)
      , ("accel", accelSender div_accel_meas)
      , ("gps", positionSender (snd position))
      ]
    ]
  schedule uartTasks systemInit uartout

  case px4platform_can px4platform of
    Nothing -> return () -- don't send sensor readings to non-existent CAN busses
    Just can -> do
      (_, canReqMbox1, canReqMbox2, canReqMbox3) <- canTower tocc
            (can_periph can) 500000 (can_RX can) (can_TX can)
      tasks <- sequence
        [ do
            (t, tx) <- canTask
            go tx
            return t
        | go <-
            [ fragmentSenderBlind gyro_meas gyroType
            , fragmentSenderBlind accel_meas accelType
            , fragmentSenderBlind mag_meas magType
            , fragmentSenderBlind baro_meas baroType
            , fragmentSenderBlind (snd position) gpsType
            ]
        ]
      canScheduler [canReqMbox1, canReqMbox2, canReqMbox3] tasks
  serializeTowerDeps

