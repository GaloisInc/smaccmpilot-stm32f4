{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32F405.SPI

import SMACCMPilot.Hardware.MPU6000.Regs
import SMACCMPilot.Hardware.MPU6000.RawSensor

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
                      -> ITime
                      -> Ivory eff (ConstRef (Stack s) (Struct "mpu6000_raw_sensor"))
rawSensorFromResponse res t = do
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
  fmap constRef $ local $ istruct
    [ valid   .= ival true
    , time    .= ival t
    , gyro_x  .= ival (hilo gx_h gx_l)
    , gyro_y  .= ival (hilo gy_h gy_l)
    , gyro_z  .= ival (hilo gz_h gz_l)
    , accel_x .= ival (hilo ax_h ax_l)
    , accel_y .= ival (hilo ay_h ay_l)
    , accel_z .= ival (hilo az_h az_l)
    , temp    .= ival (hilo te_h te_l)
    ]
  where
  hilo :: Uint8 -> Uint8 -> Uint16
  hilo h l = ((safeCast h) * 256) + safeCast l

initializerMachine :: SPIDeviceHandle
                   -> ChannelEmitter (Struct "spi_transaction_request")
                   -> Event          (Struct "spi_transaction_result")
                   -> Task p (Runnable, Ref Global (Stored IBool))
initializerMachine dev req_emitter result_evt = do
  failed <- taskLocal "mpu6000InitFailed"

  m <- stateMachine "mpu6000InitailizerMachine" $ mdo
    b <- stateNamed "begin" $ entry $ do
      liftIvory_ $ store failed false
      goto disablei2c

    -- Disable the I2C slave device sharing pins with the SPI interface
    disablei2c <- rpc "disablei2c" (writeRegReq dev UserControl 0x10)
                                   (const (return ()))
                                   whoami

    -- Poll the WhoAmI register. Check for expected result of 0x68
    whoami <- rpc "whoami" (readRegReq dev WhoAmI)
                           (\res -> do
                                idbyte <- deref ((res ~> rx_buf) ! 1)
                                unless (idbyte ==? 0x68) (store failed true))
                           wake

    -- Wake the sensor device, use internal oscillator
    wake   <- rpc "wake" (writeRegReq dev PowerManagment1 0x00)
                         (const (return ()))
                         setscale

    -- Set gyro scale to +/- 2000 dps
    setscale <- rpc "setscale" (writeRegReq dev GyroConfig 0x18)
                               (const (return ()))
                               done

    done <- stateNamed "done" $ entry $ halt

    return b

  return (m, failed)

  where
  rpc :: String
      -> (forall s1 . Ivory (AllocEffects s1)
                       (ConstRef (Stack s1) (Struct "spi_transaction_request")))
      -> (forall s2 s3 . (ConstRef s2 (Struct "spi_transaction_result"))
            -> Ivory (AllocEffects s3) ())
      -> StateLabel
      -> MachineM StateLabel
  rpc name request resultk statek = mdo
    getter <- stateNamed ("get" ++ name) $ entry $ do
      liftIvory_ $ do
        r <- request
        emit_ req_emitter r
      goto gotResult
    gotResult <- stateNamed ("got" ++ name) $ on result_evt $ \res -> do
      liftIvory_ $ resultk res
      goto statek
    return getter



