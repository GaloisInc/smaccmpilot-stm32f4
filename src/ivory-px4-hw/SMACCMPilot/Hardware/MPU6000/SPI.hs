{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import qualified Ivory.Tower.HAL.Sensor.Gyroscope as G
import qualified Ivory.Tower.HAL.Sensor.Accelerometer as A
import Ivory.BSP.STM32.Driver.SPI
import SMACCMPilot.Hardware.MPU6000.Regs

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

sensorSample :: (GetAlloc eff ~ Scope s)
                      => ConstRef s1 (Struct "spi_transaction_result")
                      -> Ref s2 (Struct "gyroscope_sample")
                      -> Ref s3 (Struct "accelerometer_sample")
                      -> Ivory eff ()
sensorSample res r_gyro r_accel = do
  r_time <- getTime
  rc <- deref (res ~> resultcode)
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
  comment "store sample failure"
  store (r_gyro ~> G.samplefail) (rc >? 0)
  store (r_accel ~> A.samplefail) (rc >? 0)
  comment "convert to degrees per second"
  store ((r_gyro ~> G.sample) ! 0) (hilo gx_h gx_l / 16.4)
  store ((r_gyro ~> G.sample) ! 1) (hilo gy_h gy_l / 16.4)
  store ((r_gyro ~> G.sample) ! 2) (hilo gz_h gz_l / 16.4)
  comment "convert to m/s/s by way of g"
  store ((r_accel ~> A.sample) ! 0)  (hilo ax_h ax_l / 2048.0 * 9.80665)
  store ((r_accel ~> A.sample) ! 1)  (hilo ay_h ay_l / 2048.0 * 9.80665)
  store ((r_accel ~> A.sample) ! 2)  (hilo az_h az_l / 2048.0 * 9.80665)
  comment "convert to degrees Celsius"
  r_temp <- assign (hilo te_h te_l / 340.0 + 36.53)
  store (r_gyro  ~> G.temp) r_temp
  store (r_accel ~> A.temp) r_temp
  comment "store sample time"
  store (r_gyro  ~> G.time) r_time
  store (r_accel ~> A.time) r_time
  where
  hilo :: Uint8 -> Uint8 -> IFloat
  hilo h l = safeCast $ twosComplementCast $ hiloUnsigned h l

  hiloUnsigned :: Uint8 -> Uint8 -> Uint16
  hiloUnsigned h l = (safeCast h `iShiftL` 8) .| safeCast l

mpu6000SensorManager :: BackpressureTransmit (Struct "spi_transaction_request") (Struct "spi_transaction_result")
                     -> ChanOutput (Stored ITime)
                     -> ChanInput  (Struct "gyroscope_sample")
                     -> ChanInput  (Struct "accelerometer_sample")
                     -> SPIDeviceHandle
                     -> Tower e ()
mpu6000SensorManager (BackpressureTransmit req_chan res_chan) init_chan gyro_chan accel_chan dev = do
  towerModule G.gyroscopeTypesModule
  towerDepends G.gyroscopeTypesModule
  towerModule A.accelerometerTypesModule
  towerDepends A.accelerometerTypesModule

  p <- period (Milliseconds 5) -- 200hz

  monitor "mpu6kCtl" $ do
    retries <- state "retries"
    ready <- state "ready"
    transactionPending <- state "transaction_pending"
    gyro_s <- state "gyro"
    accel_s <- state "accel"

    coroutineHandler init_chan res_chan "mpu6000" $ do
      req_e   <- emitter req_chan 1
      gyro_e  <- emitter gyro_chan 1
      accel_e <- emitter accel_chan 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req = req >>= emit req_e >> yield

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
          sensorSample (constRef res) gyro_s accel_s
          emit gyro_e (constRef gyro_s)
          emit accel_e (constRef accel_s)

    handler p "period" $ do
      req_e   <- emitter req_chan 1
      gyro_e  <- emitter gyro_chan 1
      accel_e <- emitter accel_chan 1
      callback $ const $ do
        isReady <- deref ready
        isPending <- deref transactionPending
        cond_
          [ iNot isReady ==> do
              store (gyro_s ~> G.initfail) true
              store (accel_s ~> A.initfail) true
              invalidTransaction gyro_s accel_s
              emit gyro_e (constRef gyro_s)
              emit accel_e (constRef accel_s)
          , isPending ==> do
              invalidTransaction gyro_s accel_s
              emit gyro_e (constRef gyro_s)
              emit accel_e (constRef accel_s)
          , true ==> do
              store (gyro_s ~> G.initfail) false
              store (accel_s ~> A.initfail) false
              store transactionPending true
              req <- getSensorsReq dev
              emit req_e req
          ]

  where
  invalidTransaction :: (GetAlloc eff ~ Scope s)
                => Ref s1 (Struct "gyroscope_sample")
                -> Ref s2 (Struct "accelerometer_sample")
                -> Ivory eff ()
  invalidTransaction r_gyro r_accel = do
    t <- getTime
    store (r_gyro ~> G.samplefail)    true
    store ((r_gyro ~> G.sample) ! 0)  0
    store ((r_gyro ~> G.sample) ! 1)  0
    store ((r_gyro ~> G.sample) ! 2)  0
    store (r_gyro ~> G.temp)          0
    store (r_gyro ~> G.time)          t
    store (r_accel ~> A.samplefail)   true
    store ((r_accel ~> A.sample) ! 0) 0
    store ((r_accel ~> A.sample) ! 1) 0
    store ((r_accel ~> A.sample) ! 2) 0
    store (r_accel ~> A.temp)         0
    store (r_accel ~> A.time)         t
