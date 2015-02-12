{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.HMC5883L.I2C where

import Data.Word
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.HMC5883L.Regs
import SMACCMPilot.Hardware.HMC5883L.Types

hmc5883lSensorManager :: ChanInput  (Struct "i2c_transaction_request")
                      -> ChanOutput (Struct "i2c_transaction_result")
                      -> ChanOutput (Stored ITime)
                      -> ChanInput  (Struct "hmc5883l_sample")
                      -> I2CDeviceAddr
                      -> Tower e ()
hmc5883lSensorManager req_chan res_chan init_chan sensor_chan addr = do
  towerModule  hmc5883lTypesModule
  towerDepends hmc5883lTypesModule
  p <- period (Milliseconds 20) -- 50 hz. Can be faster if required.
  monitor "hmc5883lSensorManager" $ do
    failure     <- stateInit "failure" (ival false)
    initialized <- stateInit "initialized" (ival false)
    s           <- state "sample"
    coroutineHandler init_chan res_chan "hmc5883l" $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensor_chan 1
      return $ CoroutineBody $ \yield -> do
        let writeReg reg val = do
              regWriteRequest addr reg val >>= emit req_e
              res <- yield
              code <- deref (res ~> resultcode)
              -- Set the initfail field if i2c failed
              when (code >? 0) (store (s ~> initfail) true)
        comment "entry to hmc5883l coroutine"
        writeReg ConfA $ confAVal Average8 Rate75Hz NoBias
        writeReg ConfB $ confBVal LSBGauss1370
        writeReg Mode  $ modeVal  Continuous
        store initialized true
        comment "finished initializing in hmc5883l coroutine"

        forever $ do
          -- Request originates from period below
          setup_read_result <- yield
          rc <- deref (setup_read_result ~> resultcode)
          -- Reset the samplefail field
          store (s ~> samplefail) (rc >? 0)
          -- Send request to perform read
          do_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit req_e do_read_req
          res <- yield
          -- Unpack read, updating samplefail if failed.
          rc2 <- deref (res ~> resultcode)
          when (rc2 >? 0) (store (s ~> samplefail) true)
          payloadu16 res 0 1 >>= store ((s ~> sample) ! 0) -- xh, xl
          payloadu16 res 2 3 >>= store ((s ~> sample) ! 2) -- zh, zl
          payloadu16 res 4 5 >>= store ((s ~> sample) ! 1) -- yh, yl
          getTime >>= store (s ~> time)
          -- Send the sample upstream.
          emit sens_e (constRef s)


    handler p "periodic_read" $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        i <- deref initialized
        when i $ do
          -- Initiate a read
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [ ival (fromIntegral (regAddr OutXH)) ]
            , tx_len  .= ival 1
            , rx_len  .= ival 0
            ]
          emit req_e setup_read_req
  where
  payloadu16 :: Ref s (Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff IFloat
  payloadu16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ safeCast (twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo) :: Sint16) / 1370.0

regWriteRequest :: (GetAlloc eff ~ Scope s)
           => I2CDeviceAddr
           -> Reg
           -> Word8
           -> Ivory eff (ConstRef (Stack s) (Struct "i2c_transaction_request"))
regWriteRequest addr r v = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (fromIntegral (regAddr r)), ival (fromIntegral v) ]
  , tx_len  .= ival 2
  , rx_len  .= ival 0
  ]

