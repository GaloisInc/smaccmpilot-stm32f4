{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MS5611.I2C where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32F4.I2C

import SMACCMPilot.Hardware.MS5611.Regs

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

promRead :: I2CDeviceAddr
         -> PROM
         -> Ref Global (Stored IBool)
         -> Ref Global (Stored Uint16)
         -> ChannelEmitter (Struct "i2c_transaction_request")
         -> Event          (Struct "i2c_transaction_result")
         -> StateLabel
         -> MachineM StateLabel
promRead i2caddr prom failure value req_emitter res_evt next = mdo
  cmdReq <- stateNamed (named "cmdReq") $ do
    entry $ liftIvory_ $ do
      -- send an i2c command to setup prom read
      req <- commandRequest i2caddr (PromRead prom)
      emit_ req_emitter req
    on res_evt $ \res -> do
      liftIvory_ $ checki2csuccess res
      -- continue to readreq state
      goto readReq

  readReq <- stateNamed (named "readReq") $ do
    entry $ liftIvory_ $ do
      -- Read prom field
      req <- promFetchRequest i2caddr
      emit_ req_emitter req
    on res_evt $ \res -> do
      liftIvory_ $ do
        checki2csuccess res
        hi <- deref ((res ~> rx_buf) ! 0)
        lo <- deref ((res ~> rx_buf) ! 1)
        u16 <- assign $ ((safeCast hi) * 256) + safeCast lo
        store value u16
      goto next

  return cmdReq
  where
  named n = "prom" ++ show prom ++ n
  checki2csuccess :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  checki2csuccess res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)


data MS5611DeviceProps =
  MS5611DeviceProps
    { init_failure :: ConstRef Global (Stored IBool)
    , coeff1       :: ConstRef Global (Stored Uint16)
    , coeff2       :: ConstRef Global (Stored Uint16)
    , coeff3       :: ConstRef Global (Stored Uint16)
    , coeff4       :: ConstRef Global (Stored Uint16)
    , coeff5       :: ConstRef Global (Stored Uint16)
    , coeff6       :: ConstRef Global (Stored Uint16)
    }

type PressureSample = Uint32
type TemperatureSample = Uint32

driverMachine :: I2CDeviceAddr
              -> ChannelEmitter (Struct "i2c_transaction_request")
              -> Event          (Struct "i2c_transaction_result")
              -> (forall eff . IBool -> PressureSample -> TemperatureSample
                            -> Ivory eff ())
              -> Task p (Runnable, MS5611DeviceProps)
driverMachine addr req_emitter res_evt sample_k = do
  init_f <- taskLocal "init_failure"
  c1     <- taskLocal "coeff1"
  c2     <- taskLocal "coeff2"
  c3     <- taskLocal "coeff3"
  c4     <- taskLocal "coeff4"
  c5     <- taskLocal "coeff5"
  c6     <- taskLocal "coeff6"
  let props = MS5611DeviceProps
        { init_failure = constRef init_f
        , coeff1       = constRef c1
        , coeff2       = constRef c2
        , coeff3       = constRef c3
        , coeff4       = constRef c4
        , coeff5       = constRef c5
        , coeff6       = constRef c6
        }

  samp_f <- taskLocal "sample_failure"
  samp_p <- taskLocal "sample_pressure"
  samp_t <- taskLocal "sample_temperature"
  let check_sample_failure :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
      check_sample_failure res = do
          r <- deref (res ~> resultcode)
          when (r >? 0) (store samp_f true)

      send_sample :: Ivory eff ()
      send_sample = do
        f <- deref samp_f
        p <- deref samp_p
        t <- deref samp_t
        sample_k f p t

  m <- stateMachine "ms5611DriverMachine" $ mdo
    b <- stateNamed "begin" $ do
          entry $ liftIvory_ $ do
            store init_f false
            req <- commandRequest addr Reset
            emit_ req_emitter req
          on res_evt $ \res -> do
            liftIvory_ $ do
              r <- deref (res ~> resultcode)
              when (r >? 0) (store init_f true)
            goto reset_wait

    reset_wait <- stateNamed "reset_wait" $ do
      timeout (Milliseconds 4) $ goto prom1

    prom1 <- promRead addr Coeff1 init_f c1 req_emitter res_evt prom2
    prom2 <- promRead addr Coeff2 init_f c2 req_emitter res_evt prom3
    prom3 <- promRead addr Coeff3 init_f c3 req_emitter res_evt prom4
    prom4 <- promRead addr Coeff4 init_f c4 req_emitter res_evt prom5
    prom5 <- promRead addr Coeff5 init_f c5 req_emitter res_evt prom6
    prom6 <- promRead addr Coeff6 init_f c6 req_emitter res_evt convertP


    convertP <- stateNamed "convertPressure" $ do
      entry $ liftIvory_ $ do
        store samp_f false
        req <- commandRequest addr (ConvertD1 OSR4096)
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ check_sample_failure res
        goto latchP

    latchP   <- stateNamed "latchPressure" $ do
      timeout (Milliseconds 9) $ liftIvory_ $ do
        req <- commandRequest addr ADCRead
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ check_sample_failure res
        goto readP

    readP    <- stateNamed "readPressure" $ do
      entry $ liftIvory_ $ do
        req <- adcFetchRequest addr
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ do
          check_sample_failure res
          threebytesample res >>= store samp_p
        goto convertT

    convertT <- stateNamed "convertTemperature" $ do
      entry $ liftIvory_ $ do
        store samp_f false
        req <- commandRequest addr (ConvertD2 OSR4096)
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ check_sample_failure res
        goto latchT

    latchT   <- stateNamed "latchTemperature" $ do
      timeout (Milliseconds 9) $ liftIvory_ $ do
        req <- commandRequest addr ADCRead
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ check_sample_failure res
        goto readT

    readT    <- stateNamed "readTemperature" $ do
      entry $ liftIvory_ $ do
        req <- adcFetchRequest addr
        emit_ req_emitter req
      on res_evt $ \res -> do
        liftIvory_ $ do
          check_sample_failure res
          threebytesample res >>= store samp_t
          send_sample
        goto convertP


    done <- stateNamed "done" $ entry $ halt

    return b

  return (m, props)


threebytesample :: ConstRef s (Struct "i2c_transaction_result")
                -> Ivory eff Uint32
threebytesample resp = do
  h <- deref ((resp ~> rx_buf) ! 0)
  m <- deref ((resp ~> rx_buf) ! 1)
  l <- deref ((resp ~> rx_buf) ! 2)
  assign ((safeCast h) * 65536 + (safeCast m) * 255 + (safeCast l))

