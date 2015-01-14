{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.MS5611 (ms5611Sender, app) where

import Ivory.Language
import Ivory.Serialize
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.MS5611
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  towerModule  ms5611TypesModule
  towerDepends ms5611TypesModule
  px4platform <- fmap topx4 getEnv
  let ms5611 = px4platform_ms5611_device px4platform
  (req, res) <- i2cTower tocc
                         (ms5611device_periph ms5611)
                         (ms5611device_sda ms5611)
                         (ms5611device_scl ms5611)
  measurements <- ms5611ctl req res (ms5611device_addr ms5611)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti, uarto) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 128)
  monitor "ms5611sender" $ do
    ms5611Sender measurements uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

ms5611Sender :: ChanOutput (Struct "ms5611_measurement")
             -> ChanInput (Stored Uint8)
             -> Monitor e ()
ms5611Sender meas out = do
  (buf :: Ref Global (Array 18 (Stored Uint8))) <- state "ms5611_ser_buf"
  handler meas "measurement" $ do
    e <- emitter out (2*18 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto_ buf 0 $ mpack s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 98 -- 'b' for barometer

ms5611ctl :: ChanInput  (Struct "i2c_transaction_request")
          -> ChanOutput (Struct "i2c_transaction_result")
          -> I2CDeviceAddr
          -> Tower e (ChanOutput (Struct "ms5611_measurement"))
ms5611ctl toDriver fromDriver addr = do
  measurements <- channel
  monitor "ms5611ctl" $ do

    calibration <- state "calibration"
    sample      <- state "sample"
    meas        <- state "meas"

    driver <- testDriverMachine addr toDriver fromDriver
                calibration sample meas (meas ~> initfail)
                (meas ~> sampfail) (fst measurements)
    stateMachine_onChan driver fromDriver
  return (snd measurements)

testDriverMachine :: I2CDeviceAddr
                  -> ChanInput (Struct "i2c_transaction_request")
                  -> ChanOutput (Struct "i2c_transaction_result")
                  -> Ref Global (Struct "ms5611_calibration")
                  -> Ref Global (Struct "ms5611_sample")
                  -> Ref Global (Struct "ms5611_measurement")
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> ChanInput (Struct "ms5611_measurement")
                  -> Monitor e (StateMachine e)
testDriverMachine addr i2cRequest i2cResult calibration sample meas ifail sfail mchan =
  stateMachine "ms5611TestDriver" $ mdo
    postinit <- machineStateNamed "postinit" $ timeout (Milliseconds 1) $
      machineControl $ \_ -> return (goto setup)
    setup <- sensorSetup  addr ifail calibration i2cRequest i2cResult read
    read  <- sensorSample addr sfail sample      i2cRequest i2cResult m
    m     <- machineStateNamed "ms5611_measurement" $ entry $ do
      e <- machineEmitter mchan 1
      machineControl $ \_ -> do
        measurement (constRef calibration) (constRef sample) meas
        emit e (constRef meas)
        return $ goto read
    return postinit


