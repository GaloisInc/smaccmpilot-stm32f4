{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MPU6000.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Serialize
import qualified SMACCMPilot.Comm.Ivory.Types.GyroscopeSample as G
import qualified SMACCMPilot.Comm.Ivory.Types.AccelerometerSample as A
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz as XYZ
import qualified SMACCMPilot.Hardware.MPU6000.Types as M
import Ivory.BSP.STM32.Driver.SPI
import SMACCMPilot.Hardware.MPU6000.Regs
import SMACCMPilot.Time

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

sensorSample :: ConstRef s1 (Struct "spi_transaction_result")
             -> Ref s2 (Struct "gyroscope_sample")
             -> Ref s3 (Struct "accelerometer_sample")
             -> Ivory ('Effects r b (Scope s)) ()
sensorSample res r_gyro r_accel = do
  r_time <- getTime
  rc <- deref (res ~> resultcode)
  mpu6000_r <- local (istruct [])
  packGetBE packRep (toCArray (res ~> rx_buf)) 1 mpu6000_r
  comment "store sample failure"
  store (r_gyro ~> G.samplefail) (rc >? 0)
  store (r_accel ~> A.samplefail) (rc >? 0)
  comment "convert to degrees per second"
  let to_dps x = safeCast x / 16.4
  convert ((r_gyro ~> G.sample) ~> XYZ.x) (mpu6000_r ~> M.gx) to_dps
  convert ((r_gyro ~> G.sample) ~> XYZ.y) (mpu6000_r ~> M.gy) to_dps
  convert ((r_gyro ~> G.sample) ~> XYZ.z) (mpu6000_r ~> M.gz) to_dps
  comment "convert to m/s/s by way of g"
  let to_g x = safeCast x / 2048.0 * 9.80665
  convert ((r_accel ~> A.sample) ~> XYZ.x)  (mpu6000_r ~> M.ax) to_g
  convert ((r_accel ~> A.sample) ~> XYZ.y)  (mpu6000_r ~> M.ay) to_g
  convert ((r_accel ~> A.sample) ~> XYZ.z)  (mpu6000_r ~> M.az) to_g
  comment "convert to degrees Celsius"
  t <- deref (mpu6000_r ~> M.temp)
  r_temp <- assign (safeCast t / 340.0 + 36.53)
  store (r_gyro  ~> G.temperature) r_temp
  store (r_accel ~> A.temperature) r_temp
  comment "store sample time"
  store (r_gyro  ~> G.time) (timeMicrosFromITime r_time)
  store (r_accel ~> A.time) (timeMicrosFromITime r_time)
  where

  convert to fro f = do
    v <- deref fro
    store to (f v)

mpu6000SensorManager :: BackpressureTransmit (Struct "spi_transaction_request") (Struct "spi_transaction_result")
                     -> ChanOutput (Stored ITime)
                     -> ChanInput  (Struct "gyroscope_sample")
                     -> ChanInput  (Struct "accelerometer_sample")
                     -> SPIDeviceHandle
                     -> Tower e ()
mpu6000SensorManager (BackpressureTransmit req_chan res_chan) init_chan gyro_chan accel_chan dev = do
  towerModule G.gyroscopeSampleTypesModule
  towerDepends G.gyroscopeSampleTypesModule
  towerModule A.accelerometerSampleTypesModule
  towerDepends A.accelerometerSampleTypesModule
  towerModule M.mpu6000ResponseTypesModule
  towerDepends M.mpu6000ResponseTypesModule

  -- TODO: let caller choose the bandwidth
  let lpfConfig = DLPF94Hz

  -- TODO: let caller request oversampling by an integer multiple
  let targetSampleRate = 2 * max (accelBandwidth lpfConfig) (gyroBandwidth lpfConfig)

  let (gyroSamplesPerMS, 0) = gyroSampleRate lpfConfig `divMod` 1000
  let samplePeriodMS = 1000 `div` targetSampleRate
  let divisor = samplePeriodMS * gyroSamplesPerMS

  p <- period (Milliseconds samplePeriodMS)

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

        comment $ "accel bandwidth: " ++ show (accelBandwidth lpfConfig :: Int) ++ "Hz, "
               ++ "gyro bandwidth: " ++ show (gyroBandwidth lpfConfig :: Int) ++ "Hz, "
               ++ "gyro sample rate: " ++ show (gyroSampleRate lpfConfig :: Int) ++ "Hz"
        _ <- rpc $ writeRegReq dev Config $ configRegVal lpfConfig

        comment $ "sample rate: " ++ show (1000 / fromInteger samplePeriodMS :: Double) ++ "Hz"
        _ <- rpc $ writeRegReq dev SampleRateDivider $ fromInteger divisor

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
    store (r_gyro ~> G.samplefail)         true
    store ((r_gyro ~> G.sample) ~> XYZ.x)  0
    store ((r_gyro ~> G.sample) ~> XYZ.y)  0
    store ((r_gyro ~> G.sample) ~> XYZ.z)  0
    store (r_gyro ~> G.temperature)        0
    store (r_gyro ~> G.time)               (timeMicrosFromITime t)
    store (r_accel ~> A.samplefail)        true
    store ((r_accel ~> A.sample) ~> XYZ.x) 0
    store ((r_accel ~> A.sample) ~> XYZ.y) 0
    store ((r_accel ~> A.sample) ~> XYZ.z) 0
    store (r_accel ~> A.temperature)       0
    store (r_accel ~> A.time)              (timeMicrosFromITime t)
