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
    initialized <- state "initialized"
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

            samplei2csuccess :: Ref s (Struct "i2c_transaction_result")
                             -> Ivory eff ()
            samplei2csuccess res = do
              c <- deref (res ~> resultcode)
              when (c >? 0) (store (meas ~> sampfail) true)

            spiTransaction :: (GetAlloc eff ~ Scope s)
                           => Ivory eff (Ref s2 (Struct "i2c_transaction_result"))
                           -> Ivory eff (ConstRef s1 (Struct "i2c_transaction_request"))
                           -> Ivory eff (Ref s2 (Struct "i2c_transaction_result"))
            spiTransaction y req = do
              spiStartTransaction req
              spiFinishTransaction y

            spiStartTransaction :: (GetAlloc eff ~ Scope s)
                           => Ivory eff (ConstRef s1 (Struct "i2c_transaction_request"))
                           -> Ivory eff ()
            spiStartTransaction req = do
              r <- req
              emit req_e r

            spiFinishTransaction :: Ivory eff (Ref s (Struct "i2c_transaction_result"))
                                 -> Ivory eff (Ref s (Struct "i2c_transaction_result"))
            spiFinishTransaction y = do
              res <- y
              samplei2csuccess res
              return res

        arrayMap $ \ i ->
          getPROM (Coeff i) (calibration ~> coeff ! i)

        forever $ do
          store (meas ~> sampfail) false

          _ <- spiTransaction yield $ commandRequest addr (ConvertD1 OSR4096)

          -- Yield until the periodic handler runs and starts an ADCRead
          -- request.
          -- Note that the first time after init, this may happen less than
          -- ~10ms after the above transaction started, so the d1 value may be
          -- hosed.  XXX fix that afterwards.
          store initialized true
          _ <- spiFinishTransaction yield

          adc_d1_read  <- spiTransaction yield $ adcFetchRequest addr

          press <- threebytesample (constRef adc_d1_read)
          store (sample ~> sample_pressure) press

          _ <- spiTransaction yield $ commandRequest addr (ConvertD2 OSR4096)

          -- Yield until the periodic handler runs and starts an ADCRead
          -- request.
          _ <- spiFinishTransaction yield

          adc_d2_read <- spiTransaction yield $ adcFetchRequest addr

          temp <- threebytesample (constRef adc_d2_read)
          store (sample ~> sample_temperature) temp
          getTime >>= store (sample ~> sample_time)

          measurement (constRef calibration) (constRef sample) meas
          emit meas_e (constRef meas)


    handler p "periodic" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        let spiStartTransaction req = do
              r <- req
              emit req_e r
        i <- deref initialized
        when i $
          spiStartTransaction $ commandRequest addr ADCRead


----

commandRequest :: (GetAlloc eff ~ Scope s)
           => I2CDeviceAddr
           -> Command
           -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
commandRequest addr cmd = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (commandVal cmd) ]
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
  assign ((safeCast h) `iShiftL` 16 + (safeCast m) `iShiftL` 8 + (safeCast l))
