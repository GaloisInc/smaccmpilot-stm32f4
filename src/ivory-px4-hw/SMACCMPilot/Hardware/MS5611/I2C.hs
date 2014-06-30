{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.MS5611.I2C where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.MS5611.Regs
import SMACCMPilot.Hardware.MS5611.Types
import SMACCMPilot.Hardware.MS5611.Calibration (measurement)

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
         -> MachineM p StateLabel
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

sensorSetup :: I2CDeviceAddr
            -> Ref Global (Stored IBool)
            -> Ref Global (Struct "ms5611_calibration")
            -> ChannelEmitter (Struct "i2c_transaction_request")
            -> Event          (Struct "i2c_transaction_result")
            -> StateLabel
            -> MachineM p StateLabel
sensorSetup addr failure calibration req_emitter res_evt next = mdo
  b <- stateNamed "begin" $ do
        entry $ liftIvory_ $ do
          store failure false
          req <- commandRequest addr Reset
          emit_ req_emitter req
        on res_evt $ \res -> do
          liftIvory_ $ check_failure res
          goto reset_wait

  reset_wait <- stateNamed "reset_wait" $ do
    timeout (Milliseconds 4) $ goto prom1

  prom1 <- pread Coeff1 (calibration ~> coeff1) prom2
  prom2 <- pread Coeff2 (calibration ~> coeff2) prom3
  prom3 <- pread Coeff3 (calibration ~> coeff3) prom4
  prom4 <- pread Coeff4 (calibration ~> coeff4) prom5
  prom5 <- pread Coeff5 (calibration ~> coeff5) prom6
  prom6 <- pread Coeff6 (calibration ~> coeff6) next

  return b
  where

  pread coef ref nxt = promRead addr coef failure ref req_emitter res_evt nxt

  check_failure :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  check_failure res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)

sensorRead :: I2CDeviceAddr
           -> ConstRef Global (Struct "ms5611_calibration")
           -> Ref Global (Struct "ms5611_measurement")
           -> ChannelEmitter (Struct "i2c_transaction_request")
           -> Event          (Struct "i2c_transaction_result")
           -> StateLabel
           -> MachineM p StateLabel
sensorRead addr cal meas req_emitter res_evt next = mdo
  sample <- machineLocal "ms5611_sample"
  s <- sensorSample addr (meas ~> sampfail) sample req_emitter res_evt m
  m <- stateNamed "ms5611_measurement" $ entry $ do
    liftIvory_ $ measurement cal (constRef sample) meas
    goto next
  return s

sensorSample :: I2CDeviceAddr
             -> Ref Global (Stored IBool)
             -> Ref Global (Struct "ms5611_sample")
             -> ChannelEmitter (Struct "i2c_transaction_request")
             -> Event          (Struct "i2c_transaction_result")
             -> StateLabel
             -> MachineM p StateLabel
sensorSample addr failure sample req_emitter res_evt next = mdo
  convertP <- stateNamed "convertPressure" $ do
    entry $ liftIvory_ $ do
      store failure false
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
        threebytesample res >>= store (sample ~> sample_pressure)
      goto convertT

  convertT <- stateNamed "convertTemperature" $ do
    entry $ liftIvory_ $ do
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
        threebytesample res >>= store (sample ~> sample_temperature)
      goto next

  return convertP

  where
  check_sample_failure :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  check_sample_failure res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)


  threebytesample :: ConstRef s (Struct "i2c_transaction_result")
                  -> Ivory eff Uint32
  threebytesample resp = do
    h <- deref ((resp ~> rx_buf) ! 0)
    m <- deref ((resp ~> rx_buf) ! 1)
    l <- deref ((resp ~> rx_buf) ! 2)
    assign ((safeCast h) * 65536 + (safeCast m) * 255 + (safeCast l))

