{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MS5611.SPI where

import Ivory.BSP.STM32.Driver.SPI
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import SMACCMPilot.Comm.Ivory.Types.BarometerSample
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)
import SMACCMPilot.Hardware.MS5611.Mode
import SMACCMPilot.Hardware.MS5611.Regs
import SMACCMPilot.Hardware.MS5611.Types

ms5611SPISensorManager :: BackpressureTransmit (Struct "spi_transaction_request") (Struct "spi_transaction_result")
                       -> ChanOutput (Stored ITime)
                       -> ChanInput  (Struct "barometer_sample")
                       -> SPIDeviceHandle
                       -> Tower e ()
ms5611SPISensorManager (BackpressureTransmit req_chan res_chan) init_chan meas_chan h = do
  towerModule  ms5611TypesModule
  towerDepends ms5611TypesModule
  towerModule  barometerSampleTypesModule
  towerDepends barometerSampleTypesModule

  p <- period (Milliseconds 10) -- ADC conversion period.
  monitor "ms5611SPISensorManager" $ do
    sample      <- state "sample"
    calibration <- state "calibration"
    meas        <- state "measurement"
    continuationMode <- stateInit "continuationMode" (ival idle)
    coroutineHandler init_chan res_chan "ms5611" $ do
      req_e <- emitter req_chan 1
      meas_e <- emitter meas_chan 1
      return $ CoroutineBody $ \yield -> do

        let getPROM prom ref = do
              fetch_req <- ms5611_prom_fetch_req h prom
              emit req_e fetch_req
              fetch_res <- yield
              initi2csuccess fetch_res
              u16 <- ms5611_res_prom fetch_res
              store ref u16

            initi2csuccess res = do
              c <- ms5611_res_code res
              when (c >? 0) (store (meas ~> initfail) true)

            samplei2csuccess :: Ref s (Struct "spi_transaction_result")
                             -> Ivory eff ()
            samplei2csuccess res = do
              c <- ms5611_res_code res
              when (c >? 0) (store (meas ~> samplefail) true)

            transaction :: (GetAlloc eff ~ Scope s)
                        => Ivory eff (Ref s2 (Struct "spi_transaction_result"))
                        -> Ivory eff (ConstRef s1 (Struct "spi_transaction_request"))
                        -> Ivory eff (Ref s2 (Struct "spi_transaction_result"))
            transaction y req = do
              startTransaction req
              finishTransaction y

            startTransaction :: (GetAlloc eff ~ Scope s)
                             => Ivory eff (ConstRef s1 (Struct "spi_transaction_request"))
                             -> Ivory eff ()
            startTransaction req = do
              r <- req
              emit req_e r

            finishTransaction :: Ivory eff (Ref s (Struct "spi_transaction_result"))
                              -> Ivory eff (Ref s (Struct "spi_transaction_result"))
            finishTransaction y = do
              res <- y
              samplei2csuccess res
              return res

        -- Indicate to the periodic handler that we need to reset the chip.
        -- At least 3ms must pass between sending the reset and doing any other
        -- transaction. To ensure this is the case, we schedule both the reset
        -- and a following garbage transaction on the 10ms periodic clock.
        store continuationMode sendReset
        _ <- finishTransaction yield
        -- The periodic handler has sent the reset, we need to wait for the
        -- periodic handler to fire once more (starting a meaningless fetch
        -- Reserved transaction) before continuing
        store continuationMode waitReset
        _ <- finishTransaction yield
        -- Reset sequence completed, signal to the periodic handler to not
        -- start any more transactions
        store continuationMode initializing

        -- Now we're ready to read the calibration coefficients from the PROM.
        arrayMap $ \ ix ->
          getPROM (Coeff ix) (calibration ~> coeff ! ix)

        -- Start the measurement sample cycle. Requires us to convert and fetch
        -- both the temperature and pressure from the sensor.
        forever $ do
          store (meas ~> samplefail) false

          -- Start an adc conversion. We must wait ~9ms for the conversion to
          -- complete before reading
          _ <- transaction yield $ ms5611_command_req h (ConvertD1 OSR4096)

          -- Yield until the periodic handler starts an ADCRead
          -- request.
          -- Note that the first time after init, this may happen less than
          -- ~10ms after the above transaction started, so the d1 value may be
          -- hosed.  XXX fix that afterwards.
          store continuationMode running
          adc_d1_read  <- finishTransaction yield

          press <- ms5611_res_sample (constRef adc_d1_read)
          store (sample ~> sample_pressure) press

          -- Start an adc conversion. We must wait ~9ms for the conversion to
          -- complete before reading
          _ <- transaction yield $ ms5611_command_req h (ConvertD2 OSR4096)

          -- Yield until the periodic handler runs and starts an ADCRead
          -- request.
          adc_d2_read <- finishTransaction yield

          temp <- ms5611_res_sample (constRef adc_d2_read)
          store (sample ~> sample_temperature) temp
          getTime >>= store (sample ~> sample_time)

          measurement (constRef calibration) (constRef sample) meas
          emit meas_e (constRef meas)


    handler p "periodic" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        -- This handler always sends the start of a transaction for the
        -- coroutine handler above to complete.
        let startTransaction req = do
              r <- req
              emit req_e r

        -- The coroutineMode state is used to signal between the two handlers
        -- which transaction should be started.
        cm <- deref continuationMode
        cond_
          [ (cm ==? idle) ==> do
              -- Do Nothing!
              -- The coroutine will set the state to waitReset next.
              return ()
          , (cm ==? sendReset) ==> do
              -- Send a reset request to the chip. Must wait a minimum of 3ms
              -- before starting any other transactions!
              startTransaction $ ms5611_command_req h Reset
          , (cm ==? waitReset) ==> do
              -- Send a (meaningless) PROM Read transaction, signaling to the
              -- coroutine handler that 10ms has passed since the chip was reset
              startTransaction $ ms5611_prom_fetch_req h Reserved
          , (cm ==? initializing) ==> do
              -- Do Nothing!
              -- The coroutine handler is taking care of post-reset
              -- initialization, which has no timing requirements.
              return ()
          , (cm ==? running) ==> do
              -- The coroutine is currently yielded, waiting for an ADCRead
              -- to be kicked off every 10ms.
              startTransaction $ ms5611_adc_fetch_req h
          ]

ms5611_command_req :: forall s eff
                    . (GetAlloc eff ~ Scope s)
                   => SPIDeviceHandle
                   -> Command
                   -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
ms5611_command_req h cmd = fmap constRef $ local $ istruct
  [ tx_device .= ival h
  , tx_buf .= iarray [ ival (commandVal cmd) ]
  , tx_len .= ival 1
  ]

ms5611_prom_fetch_req :: forall s eff
                       . (GetAlloc eff ~ Scope s)
                      => SPIDeviceHandle
                      -> PROM
                      -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
ms5611_prom_fetch_req h p = fmap constRef $ local $ istruct
  [ tx_device .= ival h
  , tx_buf    .= iarray [ ival (commandVal (PromRead p)), ival 0, ival 0 ]
  , tx_len    .= ival 3
  ]

ms5611_adc_fetch_req :: forall s eff
                      . (GetAlloc eff ~ Scope s)
                     => SPIDeviceHandle
                     -> Ivory eff (ConstRef (Stack s) (Struct "spi_transaction_request"))
ms5611_adc_fetch_req h = fmap constRef $ local $ istruct
  [ tx_device .= ival h
  , tx_buf    .= iarray [ ival (commandVal ADCRead), ival 0, ival 0, ival 0 ]
  , tx_len    .= ival 4
  ] 

ms5611_res_code :: forall s eff
                 . Ref s (Struct "spi_transaction_result")
                -> Ivory eff Uint8
ms5611_res_code = \r -> deref (r ~> resultcode)

ms5611_res_prom :: forall s eff
                 . Ref s (Struct "spi_transaction_result")
                -> Ivory eff Uint16
ms5611_res_prom = \r -> do
  h <- deref ((r ~> rx_buf) ! 1)
  l <- deref ((r ~> rx_buf) ! 2)
  assign (u16_from_2_bytes h l)

ms5611_res_sample :: forall s eff
                   . ConstRef s (Struct "spi_transaction_result")
                  -> Ivory eff Uint32
ms5611_res_sample = \r -> do
  h <- deref ((r ~> rx_buf) ! 1)
  m <- deref ((r ~> rx_buf) ! 2)
  l <- deref ((r ~> rx_buf) ! 3)
  assign (u32_from_3_bytes h m l)


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


