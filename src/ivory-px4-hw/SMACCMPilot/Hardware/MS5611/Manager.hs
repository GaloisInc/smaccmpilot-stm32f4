{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MS5611.Manager where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.MS5611.Regs
import SMACCMPilot.Hardware.MS5611.Types
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)

-- lol, modularity
data MS5611Impl req res ident
  = MS5611Impl
      { ms5611_command_req         :: forall s eff
                                    . (GetAlloc eff ~ Scope s)
                                   => ident
                                   -> Command
                                   -> Ivory eff (ConstRef (Stack s) req)
      , ms5611_prom_fetch_req      :: forall s eff
                                    . (GetAlloc eff ~ Scope s)
                                   => ident
                                   -> Ivory eff (ConstRef (Stack s) req)
      , ms5611_adc_fetch_req       :: forall s eff
                                    . (GetAlloc eff ~ Scope s)
                                   => ident
                                   -> Ivory eff (ConstRef (Stack s) req)
      , ms5611_res_code            :: forall s eff
                                    . Ref s res
                                   -> Ivory eff Uint8
      , ms5611_res_prom            :: forall s eff
                                    . Ref s res
                                   -> Ivory eff Uint16
      , ms5611_res_sample          :: forall s eff
                                    . ConstRef s res
                                   -> Ivory eff Uint32
      }

ms5611_I2C :: MS5611Impl (Struct "i2c_transaction_request")
                         (Struct "i2c_transaction_result")
                         I2CDeviceAddr
ms5611_I2C = MS5611Impl
  { ms5611_command_req = \addr cmd -> fmap constRef $ local $ istruct
      [ tx_addr .= ival addr
      , tx_buf  .= iarray [ ival (commandVal cmd) ]
      , tx_len  .= ival 1
      , rx_len  .= ival 0
      ]
  , ms5611_prom_fetch_req = \addr -> fmap constRef $ local $ istruct
      [ tx_addr .= ival addr
      , tx_buf  .= iarray []
      , tx_len  .= ival 0
      , rx_len  .= ival 2
      ]
  , ms5611_adc_fetch_req = \addr -> fmap constRef $ local $ istruct
      [ tx_addr .= ival addr
      , tx_buf  .= iarray []
      , tx_len  .= ival 0
      , rx_len  .= ival 3
      ]
  , ms5611_res_code = \r -> deref (r ~> resultcode)
  , ms5611_res_prom = \r -> do
      h <- deref ((r ~> rx_buf) ! 0)
      l <- deref ((r ~> rx_buf) ! 1)
      assign (u16_from_2_bytes h l)
  , ms5611_res_sample = \r -> do
      h <- deref ((r ~> rx_buf) ! 0)
      m <- deref ((r ~> rx_buf) ! 1)
      l <- deref ((r ~> rx_buf) ! 2)
      assign (u32_from_3_bytes h m l)
  }


ms5611I2CSensorManager :: ChanInput  (Struct "i2c_transaction_request")
                       -> ChanOutput (Struct "i2c_transaction_result")
                       -> ChanOutput (Stored ITime)
                       -> ChanInput  (Struct "ms5611_measurement")
                       -> I2CDeviceAddr
                       -> Tower e ()
ms5611I2CSensorManager = ms5611SensorManager ms5611_I2C

ms5611SensorManager :: forall req res ident e
                     . (IvoryArea req, IvoryZero req, IvoryArea res, IvoryZero res)
                    => MS5611Impl req res ident
                    -> ChanInput  req
                    -> ChanOutput res
                    -> ChanOutput (Stored ITime)
                    -> ChanInput  (Struct "ms5611_measurement")
                    -> ident
                    -> Tower e ()
ms5611SensorManager i req_chan res_chan init_chan meas_chan addr = do
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
              read_req <- ms5611_command_req i addr (PromRead prom)
              emit req_e read_req
              read_res <- yield
              initi2csuccess read_res
              fetch_req <- ms5611_prom_fetch_req i addr
              emit req_e fetch_req
              fetch_res <- yield
              initi2csuccess fetch_res
              u16 <- ms5611_res_prom i fetch_res
              store ref u16

            initi2csuccess res = do
              c <- ms5611_res_code i res
              when (c >? 0) (store (meas ~> initfail) true)

            samplei2csuccess :: Ref s res
                             -> Ivory eff ()
            samplei2csuccess res = do
              c <- ms5611_res_code i res
              when (c >? 0) (store (meas ~> sampfail) true)

            transaction :: (GetAlloc eff ~ Scope s)
                        => Ivory eff (Ref s2 res)
                        -> Ivory eff (ConstRef s1 req)
                        -> Ivory eff (Ref s2 res)
            transaction y req = do
              startTransaction req
              finishTransaction y

            startTransaction :: (GetAlloc eff ~ Scope s)
                             => Ivory eff (ConstRef s1 req)
                             -> Ivory eff ()
            startTransaction req = do
              r <- req
              emit req_e r

            finishTransaction :: Ivory eff (Ref s res)
                              -> Ivory eff (Ref s res)
            finishTransaction y = do
              res <- y
              samplei2csuccess res
              return res

        arrayMap $ \ ix ->
          getPROM (Coeff ix) (calibration ~> coeff ! ix)

        forever $ do
          store (meas ~> sampfail) false

          _ <- transaction yield $ ms5611_command_req i addr (ConvertD1 OSR4096)

          -- Yield until the periodic handler runs and starts an ADCRead
          -- request.
          -- Note that the first time after init, this may happen less than
          -- ~10ms after the above transaction started, so the d1 value may be
          -- hosed.  XXX fix that afterwards.
          store initialized true
          _ <- finishTransaction yield

          adc_d1_read  <- transaction yield $ ms5611_adc_fetch_req i addr

          press <- ms5611_res_sample i (constRef adc_d1_read)
          store (sample ~> sample_pressure) press

          _ <- transaction yield $ ms5611_command_req i addr (ConvertD2 OSR4096)

          -- Yield until the periodic handler runs and starts an ADCRead
          -- request.
          _ <- finishTransaction yield

          adc_d2_read <- transaction yield $ ms5611_adc_fetch_req i addr

          temp <- ms5611_res_sample i (constRef adc_d2_read)
          store (sample ~> sample_temperature) temp
          getTime >>= store (sample ~> sample_time)

          measurement (constRef calibration) (constRef sample) meas
          emit meas_e (constRef meas)


    handler p "periodic" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        let startTransaction req = do
              r <- req
              emit req_e r
        ini <- deref initialized
        when ini $
          startTransaction $ ms5611_command_req i addr ADCRead


----

u32_from_3_bytes :: Uint8 -> Uint8 -> Uint8 -> Uint32
u32_from_3_bytes h m l = h' + m' + l'
  where
  h' = (safeCast h) `iShiftL` 16
  m' = (safeCast m) `iShiftL` 8
  l' = (safeCast l)

u16_from_2_bytes :: Uint8 -> Uint8 -> Uint16
u16_from_2_bytes h l = h' + l'
  where
  h' = (safeCast h) `iShiftL` 8
  l' = (safeCast l)


