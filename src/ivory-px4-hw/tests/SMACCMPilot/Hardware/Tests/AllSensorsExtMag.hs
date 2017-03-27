{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.Tests.AllSensorsExtMag
  ( app
  ) where

import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN.Fragment
import Ivory.Tower.HAL.Bus.CAN.Sched
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.CAN
import SMACCMPilot.Hardware.CANMessages

import SMACCMPilot.Hardware.HMC5883L

import SMACCMPilot.Hardware.Sensors
import SMACCMPilot.Hardware.SensorManager
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Serialize
import SMACCMPilot.Hardware.Tests.Ublox (uartUbloxGPSTower)

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  let tocc = px4platform_clockconfig . topx4
      tosens = px4platform_sensors . topx4

  let gps = px4platform_gps px4platform
  position <- channel
  mon1 <- uartUbloxGPSTower tocc gps (fst position)

  (hmc5883l_in, ext_mag_meas) <- channel

  let hmc5883l = ExternalSensor {
          ext_sens_name = "hmc5883l"
        , ext_sens_init = \bpt init_chan ->
            hmc5883lSensorManager hmcCal
              bpt init_chan hmc5883l_in (I2CDeviceAddr 0x1e)
        }
      -- this is hardcoded because the flight platform config parser
      -- is in smaccm-flight :(
      hmcCal = MagCal $ XyzCal {
          cal_x_offset = 0
        , cal_y_offset = 0
        , cal_z_offset = 0
        , cal_x_scale = 1.0
        , cal_y_scale = 1.0
        , cal_z_scale = 1.0
        }

  (accel_meas, gyro_meas, _mag_meas, baro_meas) <- sensorManager tosens tocc [ hmc5883l ]

  (uartout, _uarti, mon2) <- px4ConsoleTower topx4
  monitor "console_uart" (mon1 >> mon2)

  div_accel_meas <- rateDivider 4 accel_meas
  div_gyro_meas <- rateDivider 4 gyro_meas

  uartTasks <- sequence
    [ do
        (t, req) <- task name
        monitor name $ f req
        return t
    | (name, f) <-
      [ ("mag", magSender ext_mag_meas)
      , ("baro", baroSender baro_meas)
      , ("gyro", gyroSender div_gyro_meas)
      , ("accel", accelSender div_accel_meas)
      , ("gps", positionSender (snd position))
      ]
    ]
  schedule "uart" uartTasks systemInit uartout

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
            , fragmentSenderBlind ext_mag_meas magType
            , fragmentSenderBlind baro_meas baroType
            , fragmentSenderBlind (snd position) gpsType
            ]
        ]
      canScheduler [canReqMbox1, canReqMbox2, canReqMbox3] tasks
  serializeTowerDeps

