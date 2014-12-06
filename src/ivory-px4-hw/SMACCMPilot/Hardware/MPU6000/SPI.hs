{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32.Driver.SPI

import SMACCMPilot.Hardware.MPU6000.Regs
import SMACCMPilot.Hardware.MPU6000.Types

readRegAddr :: Reg -> Uint8
readRegAddr reg = 0x80 .| (fromIntegral (regAddr reg))

writeRegAddr :: Reg -> Uint8
writeRegAddr reg = fromIntegral (regAddr reg)

readRegReq :: (GetAlloc eff ~ Scope s)
           => SPIDeviceHandle
           -> Reg
           -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
readRegReq dev reg = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (readRegAddr reg), ival 0 ]
  , tx_len    .= ival 2
  ]

writeRegReq :: (GetAlloc eff ~ Scope s)
            => SPIDeviceHandle
            -> Reg
            -> Uint8
            -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
writeRegReq dev reg v = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (writeRegAddr reg), ival v ]
  , tx_len    .= ival 2
  ]

getSensorsReq :: (GetAlloc eff ~ Scope s)
              => SPIDeviceHandle
              -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
getSensorsReq dev = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [ ival (readRegAddr AccelXoutH) ]
  , tx_len    .= ival 15 -- addr, 6 accel, 2 temp, 6 gyro
  ]

rawSensorFromResponse :: (GetAlloc eff ~ Scope s)
                      => ConstRef s1 (Struct "spi_transaction_result")
                      -> Ref s2 (Struct "mpu6000_sample")
                      -> Ivory eff ()
rawSensorFromResponse res r = do
  t <- getTime
  ax_h <- deref ((res ~> rx_buf) ! 1)
  ax_l <- deref ((res ~> rx_buf) ! 2)
  ay_h <- deref ((res ~> rx_buf) ! 3)
  ay_l <- deref ((res ~> rx_buf) ! 4)
  az_h <- deref ((res ~> rx_buf) ! 5)
  az_l <- deref ((res ~> rx_buf) ! 6)
  te_h <- deref ((res ~> rx_buf) ! 7)
  te_l <- deref ((res ~> rx_buf) ! 8)
  gx_h <- deref ((res ~> rx_buf) ! 9)
  gx_l <- deref ((res ~> rx_buf) ! 10)
  gy_h <- deref ((res ~> rx_buf) ! 11)
  gy_l <- deref ((res ~> rx_buf) ! 12)
  gz_h <- deref ((res ~> rx_buf) ! 13)
  gz_l <- deref ((res ~> rx_buf) ! 14)
  store (r ~> samplefail)  false
  -- convert to degrees per second
  store (r ~> gyro_x)      (hilo gx_h gx_l / 16.4)
  store (r ~> gyro_y)      (hilo gy_h gy_l / 16.4)
  store (r ~> gyro_z)      (hilo gz_h gz_l / 16.4)
  -- convert to m/s/s by way of g
  store (r ~> accel_x)     (hilo ax_h ax_l / 2048.0 * 9.80665)
  store (r ~> accel_y)     (hilo ay_h ay_l / 2048.0 * 9.80665)
  store (r ~> accel_z)     (hilo az_h az_l / 2048.0 * 9.80665)
  -- convert to degrees Celsius
  store (r ~> temp)        (hilo te_h te_l / 340.0 + 36.53)
  store (r ~> time)        t
  where
  hilo :: Uint8 -> Uint8 -> IFloat
  hilo h l = safeCast $ twosComplementCast $ hiloUnsigned h l

  hiloUnsigned :: Uint8 -> Uint8 -> Uint16
  hiloUnsigned h l = (safeCast h `iShiftL` 8) .| safeCast l

initializerMachine :: forall e
                    . SPIDeviceHandle
                   -> ChanInput  (Struct "spi_transaction_request")
                   -> ChanOutput (Struct "spi_transaction_result")
                   -> Monitor e (StateMachine e, Ref Global (Stored IBool))
initializerMachine dev req_chan res_chan = do
  retries <- state "mpu6000InitRetries"
  failed <- state "mpu6000InitFailed"

  m <- stateMachine "mpu6000InitailizerMachine" $ mdo
    b <- machineStateNamed "begin" $ timeout (ms 1) $ do
      machineControl $ \_ -> do
        store retries (0 :: Uint8)
        return $ goto disablei2c

    -- Disable the I2C slave device sharing pins with the SPI interface
    disablei2c <- rpc "disablei2c" (writeRegReq dev UserControl 0x10)
                                   (const (return ()))
                                   whoami

    -- Poll the WhoAmI register. Check for expected result of 0x68
    whoami <- rpc "whoami" (readRegReq dev WhoAmI)
                           (\res -> do
                                idbyte <- deref ((res ~> rx_buf) ! 1)
                                store failed (iNot (idbyte ==? 0x68)))
                           wake

    -- Wake the sensor device, use internal oscillator
    wake   <- rpc "wake" (writeRegReq dev PowerManagment1 0x00)
                         (const (return ()))
                         setaccelscale

    -- Set accelerometer scale to +/- 16g
    setaccelscale <- rpc "setaccelscale" (writeRegReq dev AccelConfig 0x18)
                                         (const (return ()))
                                         setgyroscale

    -- Set gyro scale to +/- 2000 dps
    setgyroscale <- rpc "setgyroscale" (writeRegReq dev GyroConfig 0x18)
                                       (const (return ()))
                                       done

    done <- machineStateNamed "done" $ entry $ machineControl $ \_ -> do
      fd <- deref failed
      rs <- deref retries
      store retries (rs + 1)
      return $ do
        haltWhen (iNot fd)
        haltWhen (rs >? 2)
        goto disablei2c

    return b

  stateMachine_onChan m res_chan
  return (m, failed)

  where
  rpc :: String
      -> (forall s1 . Ivory (AllocEffects s1)
                       (ConstRef (Stack s1) (Struct "spi_transaction_request")))
      -> (forall s2 s3 . (ConstRef s2 (Struct "spi_transaction_result"))
            -> Ivory (AllocEffects s3) ())
      -> StateLabel
      -> MachineM p StateLabel
  rpc name request resultk statek = mdo
    getter <- machineStateNamed ("get" ++ name) $ entry $ do
      req_emitter <- machineEmitter req_chan 1
      machineControl $ \_ -> do
        r <- request
        emit req_emitter r
        return $ goto gotResult
    gotResult <- machineStateNamed ("got" ++ name) $ on res_chan $ do
      machineControl $ \res -> do
        resultk res
        return $ goto statek
    return getter


