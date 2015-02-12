{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MS5611.I2C where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.MS5611.Regs
import SMACCMPilot.Hardware.MS5611.Types
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)


ms5611SensorManager :: ChanInput  (Struct "i2c_transaction_request")
                    -> ChanOutput (Struct "i2c_transaction_result")
                    -> ChanOutput (Stored ITime)
                    -> ChanInput  (Struct "ms5611_measurement")
                    -> I2CDeviceAddr
                    -> Tower e ()
ms5611SensorManager req_chan res_chan init_chan meas_chan addr = do
  towerModule  ms5611TypesModule
  towerDepends ms5611TypesModule

  p <- period (Milliseconds 10) -- ADC conversion period.
  monitor "ms5611SensorManager" $ do
    sample      <- state "sample"
    calibration <- state "calibration"
    meas        <- state "measurement"
    phase       <- stateInit "phase" (ival (0 :: Uint32))
    coroutineHandler init_chan res_chan "ms5611" $ do
      req_e <- emitter req_chan 1
      meas_e <- emitter meas_chan 1
      return $ CoroutineBody $ \yield -> do

        let getPROM prom ref = do
              read_req <- commandRequest addr (PromRead prom)
              emit req_e read_req
              read_res <- yield
              initi2csuccess read_res
              fetch_req <- promFetchRequest addr
              emit req_e fetch_req
              fetch_res <- yield
              initi2csuccess fetch_res
              hi <- deref ((fetch_res ~> rx_buf) ! 0)
              lo <- deref ((fetch_res ~> rx_buf) ! 1)
              u16 <- assign (((safeCast hi) * 256) + safeCast lo)
              store ref u16

            initi2csuccess res = do
              c <- deref (res ~> resultcode)
              when (c >? 0) (store (meas ~> initfail) true)
            samplei2csuccess res = do
              c <- deref (res ~> resultcode)
              when (c >? 0) (store (meas ~> sampfail) true)

        getPROM Coeff1 (calibration ~> coeff1)
        getPROM Coeff2 (calibration ~> coeff2)
        getPROM Coeff3 (calibration ~> coeff3)
        getPROM Coeff4 (calibration ~> coeff4)
        getPROM Coeff5 (calibration ~> coeff5)
        getPROM Coeff6 (calibration ~> coeff6)

        forever $ do
          store (meas ~> sampfail) false
          store phase 1
          -- convert_d1_req is kicked off by periodic handler below
          convert_d1_res <- yield
          samplei2csuccess convert_d1_res

          read_d1_req <- commandRequest addr ADCRead
          emit req_e read_d1_req
          read_d1_res <- yield
          samplei2csuccess read_d1_res

          adc_d1_req <- adcFetchRequest addr
          emit req_e adc_d1_req
          adc_d1_res <- yield
          samplei2csuccess adc_d1_res

          press <- threebytesample (constRef adc_d1_res)
          store (sample ~> sample_pressure) press

          store phase 2
          -- convert_d2_req is kicked off by periodic handler below
          convert_d2_res <- yield
          samplei2csuccess convert_d2_res

          read_d2_req <- commandRequest addr ADCRead
          emit req_e read_d2_req
          read_d2_res <- yield
          samplei2csuccess read_d2_res

          adc_d2_req <- adcFetchRequest addr
          emit req_e adc_d2_req
          adc_d2_res <- yield
          samplei2csuccess adc_d2_res

          temp <- threebytesample (constRef adc_d2_res)
          store (sample ~> sample_temperature) temp
          getTime >>= store (sample ~> sample_time)

          measurement (constRef calibration) (constRef sample) meas
          emit meas_e (constRef meas)


    handler p "periodic" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        ph <- deref phase
        cond_
          [ (ph ==? 1) ==> do
              convert_d1_req <- commandRequest addr (ConvertD1 OSR4096)
              emit req_e convert_d1_req
          , (ph ==? 2) ==> do
              convert_d2_req <- commandRequest addr (ConvertD2 OSR4096)
              emit req_e convert_d2_req
          ]


----

commandRequest :: (GetAlloc eff ~ Scope s)
           => I2CDeviceAddr
           -> Command
           -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
commandRequest addr cmd = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (fromIntegral (commandVal cmd)) ]
  , tx_len  .= ival 1
  , rx_len  .= ival 0
  ]

promFetchRequest :: (GetAlloc eff ~ Scope s)
            => I2CDeviceAddr
            -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
promFetchRequest addr = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray []
  , tx_len  .= ival 0
  , rx_len  .= ival 2
  ]

adcFetchRequest :: (GetAlloc eff ~ Scope s)
            => I2CDeviceAddr
            -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
adcFetchRequest addr = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray []
  , tx_len  .= ival 0
  , rx_len  .= ival 3
  ]

threebytesample :: ConstRef s (Struct "i2c_transaction_result")
                -> Ivory eff Uint32
threebytesample resp = do
  h <- deref ((resp ~> rx_buf) ! 0)
  m <- deref ((resp ~> rx_buf) ! 1)
  l <- deref ((resp ~> rx_buf) ! 2)
  assign ((safeCast h) * 65536 + (safeCast m) * 255 + (safeCast l))
