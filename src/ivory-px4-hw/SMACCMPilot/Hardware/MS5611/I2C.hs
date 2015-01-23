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
           -> Ix 128
           -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
commandRequest addr cmd toRead = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (fromIntegral (commandVal cmd)) ]
  , tx_len  .= ival 1
  , rx_len  .= ival toRead
  ]

promFetchRequest :: (GetAlloc eff ~ Scope s)
            => I2CDeviceAddr
            -> PROM
            -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
promFetchRequest addr prom = commandRequest addr (PromRead prom) 2

adcFetchRequest :: (GetAlloc eff ~ Scope s)
            => I2CDeviceAddr
            -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
adcFetchRequest addr = commandRequest addr ADCRead 3

promRead :: I2CDeviceAddr
         -> PROM
         -> Ref Global (Stored IBool)
         -> Ref Global (Stored Uint16)
         -> ChanInput (Struct "i2c_transaction_request")
         -> ChanOutput (Struct "i2c_transaction_result")
         -> StateLabel
         -> MachineM p StateLabel
promRead i2caddr prom failure value req_chan res_evt next =
  machineStateNamed (named "readReq") $ do
    entry $ do
      req_emitter <- machineEmitter req_chan 1
      machineCallback $ \_ -> do
        -- Read prom field
        req <- promFetchRequest i2caddr prom
        emit req_emitter req
    on res_evt $ machineControl $ \res -> do
      checki2csuccess res
      hi <- deref ((res ~> rx_buf) ! 0)
      lo <- deref ((res ~> rx_buf) ! 1)
      u16 <- assign $ ((safeCast hi) * 256) + safeCast lo
      store value u16
      return $ goto next
  where
  named n = "prom" ++ show prom ++ n
  checki2csuccess :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  checki2csuccess res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)

ms5611ctl :: ChanInput  (Struct "i2c_transaction_request")
          -> ChanOutput (Struct "i2c_transaction_result")
          -> I2CDeviceAddr
          -> Tower e (ChanOutput (Struct "ms5611_measurement"))
ms5611ctl toDriver fromDriver addr = do
  measurements <- channel
  monitor "ms5611ctl" $ do

    calibration <- state "calibration"
    sample      <- state "sample"
    meas        <- state "meas"

    driver <- testDriverMachine addr toDriver fromDriver
                calibration sample meas (meas ~> initfail)
                (meas ~> sampfail) (fst measurements)
    stateMachine_onChan driver fromDriver
  return (snd measurements)

sensorSetup :: I2CDeviceAddr
            -> Ref Global (Stored IBool)
            -> Ref Global (Struct "ms5611_calibration")
            -> ChanInput (Struct "i2c_transaction_request")
            -> ChanOutput (Struct "i2c_transaction_result")
            -> StateLabel
            -> MachineM p StateLabel
sensorSetup addr failure calibration req_chan res_evt next = mdo
  b <- machineStateNamed "begin" $ do
        entry $ do
          req_emitter <- machineEmitter req_chan 1
          machineCallback $ \_ -> do
            store failure false
            req <- commandRequest addr Reset 0
            emit req_emitter req
        on res_evt $ machineControl $ \res -> do
          check_failure res
          return $ goto reset_wait

  reset_wait <- machineStateNamed "reset_wait" $ do
    timeout (Milliseconds 4) $ goto prom1

  prom1 <- pread Coeff1 (calibration ~> coeff1) prom2
  prom2 <- pread Coeff2 (calibration ~> coeff2) prom3
  prom3 <- pread Coeff3 (calibration ~> coeff3) prom4
  prom4 <- pread Coeff4 (calibration ~> coeff4) prom5
  prom5 <- pread Coeff5 (calibration ~> coeff5) prom6
  prom6 <- pread Coeff6 (calibration ~> coeff6) next

  return b
  where

  pread coef ref nxt = promRead addr coef failure ref req_chan res_evt nxt

  check_failure :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  check_failure res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)

sensorRead :: I2CDeviceAddr
           -> ConstRef Global (Struct "ms5611_calibration")
           -> Ref Global (Struct "ms5611_measurement")
           -> ChanInput (Struct "i2c_transaction_request")
           -> ChanOutput (Struct "i2c_transaction_result")
           -> StateLabel
           -> MachineM p StateLabel
sensorRead addr cal meas req_emitter res_evt next = mdo
  sample <- machineLocal "ms5611_sample"
  s <- sensorSample addr (meas ~> sampfail) sample req_emitter res_evt m
  m <- machineStateNamed "ms5611_measurement" $ entry $ machineControl $ \_ -> do
      measurement cal (constRef sample) meas
      return $ goto next
  return s

sensorSample :: I2CDeviceAddr
             -> Ref Global (Stored IBool)
             -> Ref Global (Struct "ms5611_sample")
             -> ChanInput (Struct "i2c_transaction_request")
             -> ChanOutput (Struct "i2c_transaction_result")
             -> StateLabel
             -> MachineM p StateLabel
sensorSample addr failure sample req_chan res_evt next = mdo
  convertP <- machineStateNamed "convertPressure" $ do
    entry $ do
      req_emitter <- machineEmitter req_chan 1
      machineCallback $ \_ -> do
        store failure false
        req <- commandRequest addr (ConvertD1 OSR4096) 0
        emit req_emitter req
    on res_evt $ machineControl $ \res -> do
      check_sample_failure res
      return $ goto readP

  readP    <- machineStateNamed "readPressure" $ do
    timeout (Milliseconds 10) $ do
      req_emitter <- machineEmitter req_chan 1
      machineCallback $ \_ -> do
        req <- adcFetchRequest addr
        emit req_emitter req
    on res_evt $ machineControl $ \res -> do
      check_sample_failure res
      threebytesample res >>= store (sample ~> sample_pressure)
      return $ goto convertT

  convertT <- machineStateNamed "convertTemperature" $ do
    entry $ do
      req_emitter <- machineEmitter req_chan 1
      machineCallback $ \_ -> do
        req <- commandRequest addr (ConvertD2 OSR4096) 0
        emit req_emitter req
    on res_evt $ machineControl $ \res -> do
      check_sample_failure res
      return $ goto readT

  readT    <- machineStateNamed "readTemperature" $ do
    timeout (Milliseconds 10) $ do
      req_emitter <- machineEmitter req_chan 1
      machineCallback $ \_ -> do
        req <- adcFetchRequest addr
        emit req_emitter req
    on res_evt $ machineControl $ \res -> do
      check_sample_failure res
      threebytesample res >>= store (sample ~> sample_temperature)
      getTime >>= store (sample ~> sample_time)
      return $ goto next

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

testDriverMachine :: I2CDeviceAddr
                  -> ChanInput (Struct "i2c_transaction_request")
                  -> ChanOutput (Struct "i2c_transaction_result")
                  -> Ref Global (Struct "ms5611_calibration")
                  -> Ref Global (Struct "ms5611_sample")
                  -> Ref Global (Struct "ms5611_measurement")
                  -> Ref Global (Stored IBool)
                  -> Ref Global (Stored IBool)
                  -> ChanInput (Struct "ms5611_measurement")
                  -> Monitor e (StateMachine e)
testDriverMachine addr i2cRequest i2cResult calibration sample meas ifail sfail mchan =
  stateMachine "ms5611TestDriver" $ mdo
    postinit <- machineStateNamed "postinit" $ timeout (Milliseconds 1) $
      machineControl $ \_ -> return (goto setup)
    setup <- sensorSetup  addr ifail calibration i2cRequest i2cResult read
    read  <- sensorSample addr sfail sample      i2cRequest i2cResult m
    m     <- machineStateNamed "ms5611_measurement" $ entry $ do
      e <- machineEmitter mchan 1
      machineControl $ \_ -> do
        measurement (constRef calibration) (constRef sample) meas
        emit e (constRef meas)
        return $ goto read
    return postinit
