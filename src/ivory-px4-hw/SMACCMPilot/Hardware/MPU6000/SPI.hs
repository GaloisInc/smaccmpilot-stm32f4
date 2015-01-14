{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
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
  comment "convert to degrees per second"
  store (r ~> gyro_x)      (hilo gx_h gx_l / 16.4)
  store (r ~> gyro_y)      (hilo gy_h gy_l / 16.4)
  store (r ~> gyro_z)      (hilo gz_h gz_l / 16.4)
  comment "convert to m/s/s by way of g"
  store (r ~> accel_x)     (hilo ax_h ax_l / 2048.0 * 9.80665)
  store (r ~> accel_y)     (hilo ay_h ay_l / 2048.0 * 9.80665)
  store (r ~> accel_z)     (hilo az_h az_l / 2048.0 * 9.80665)
  comment "convert to degrees Celsius"
  store (r ~> temp)        (hilo te_h te_l / 340.0 + 36.53)
  store (r ~> time)        t
  where
  hilo :: Uint8 -> Uint8 -> IFloat
  hilo h l = safeCast $ twosComplementCast $ hiloUnsigned h l

  hiloUnsigned :: Uint8 -> Uint8 -> Uint16
  hiloUnsigned h l = (safeCast h `iShiftL` 8) .| safeCast l

mpu6000SensorManager :: ChanInput  (Struct "spi_transaction_request")
                     -> ChanOutput (Struct "spi_transaction_result")
                     -> ChanInput  (Struct "mpu6000_sample")
                     -> SPIDeviceHandle
                     -> Tower e ()
mpu6000SensorManager req_chan res_chan sensorChan dev = do
  p <- period (Milliseconds 5) -- 200hz
  (doStartChan, resetChan) <- channel

  monitor "mpu6kCtl" $ do
    retries <- state "retries"
    ready <- state "ready"
    transactionPending <- state "transaction_pending"
    result <- state "result"

    coroutineHandler resetChan res_chan $ do
      reqEmitter <- emitter req_chan 1
      sensorEmitter <- emitter sensorChan 1
      return $ coroutine $ \ yield -> proc "mpu6000" $ body $ do
        let rpc req = req >>= emit reqEmitter >> yield

        store retries (0 :: Uint8)
        forever $ do
          comment "Disable the I2C slave device sharing pins with the SPI interface"
          _ <- noBreak $ rpc (writeRegReq dev UserControl 0x10)

          comment "Poll the WhoAmI register"
          whoami <- noBreak $ rpc (readRegReq dev WhoAmI)
          idbyte <- deref ((whoami ~> rx_buf) ! 1)

          comment "Device is working when WhoAmI returns 0x68"
          when (idbyte ==? 0x68) breakOut

          rs <- deref retries
          store retries (rs + 1)
          when (rs >? 2) retVoid

        comment "Wake the sensor device, use internal oscillator"
        _ <- rpc (writeRegReq dev PowerManagment1 0x00)

        comment "Set accelerometer scale to +/- 16g"
        _ <- rpc (writeRegReq dev AccelConfig 0x18)

        comment "Set gyro scale to +/- 2000 dps"
        _ <- rpc (writeRegReq dev GyroConfig 0x18)

        store ready true
        forever $ do
          comment "Wait for responses to periodic requests"
          res <- yield
          comment "Got a response, sending it up the stack"
          store transactionPending false
          rawSensorFromResponse (constRef res) result
          emit sensorEmitter $ constRef result

    started <- state "started"

    handler p "period" $ do
      doStart <- emitter doStartChan 1
      spiEmitter <- emitter req_chan 1
      sensorEmitter <- emitter sensorChan 1
      callback $ const $ do
        isReady <- deref ready
        isPending <- deref transactionPending
        cond_
          [ iNot isReady ==> do
              store (result ~> initfail) true
              invalidTransaction result
              emit sensorEmitter (constRef result)
              alreadyStarted <- deref started
              unless alreadyStarted $ do
                emitV doStart true
                store started true
          , isPending ==> do
              invalidTransaction result
              emit sensorEmitter (constRef result)
          , true ==> do
              store (result ~> initfail) false
              store transactionPending true
              req <- getSensorsReq dev
              emit spiEmitter req
          ]

  where
  invalidTransaction :: (GetAlloc eff ~ Scope s)
                => Ref s' (Struct "mpu6000_sample") -> Ivory eff ()
  invalidTransaction r = do
    t <- getTime
    store (r ~> samplefail) true
    store (r ~> gyro_x)     0
    store (r ~> gyro_y)     0
    store (r ~> gyro_z)     0
    store (r ~> accel_x)    0
    store (r ~> accel_y)    0
    store (r ~> accel_z)    0
    store (r ~> temp)       0
    store (r ~> time)       t
