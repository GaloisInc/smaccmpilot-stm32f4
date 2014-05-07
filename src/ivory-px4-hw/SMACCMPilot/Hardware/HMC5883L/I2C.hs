{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Hardware.HMC5883L.I2C where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.BSP.STM32F4.I2C

import SMACCMPilot.Hardware.HMC5883L.Regs

import Data.Word

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


sensorSetup :: I2CDeviceAddr
         -> Ref Global (Stored IBool)
         -> ChannelEmitter (Struct "i2c_transaction_request")
         -> Event          (Struct "i2c_transaction_result")
         -> StateLabel
         -> MachineM StateLabel
sensorSetup i2caddr failure req_emitter res_evt next = mdo
  writeConfA <- writeReg ConfA confa writeConfB
  writeConfB <- writeReg ConfB confb writeMode
  writeMode  <- writeReg Mode  mode  next
  return writeConfA
  where
  confa = confAVal Average8 Rate75Hz NoBias
  confb = confBVal LSBGauss1370
  mode  = modeVal  Continious

  named n = "hmc8553l_" ++ n

  writeReg reg val nextstate = stateNamed (named ("write" ++ show reg)) $ do
    entry $ liftIvory_ $ do
      req <- regWriteRequest i2caddr reg val
      emit_ req_emitter req
    on res_evt $ \res -> do
      liftIvory_ $ checki2csuccess res
      goto nextstate

  checki2csuccess :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  checki2csuccess res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)

sensorRead :: I2CDeviceAddr
         -> Ref Global (Stored IBool)
         -> Ref Global (Array 3 (Stored Uint16))
         -> ChannelEmitter (Struct "i2c_transaction_request")
         -> Event          (Struct "i2c_transaction_result")
         -> StateLabel
         -> MachineM StateLabel
sensorRead i2caddr failure value req_emitter res_evt next = mdo
  readSetup <- stateNamed (named "read_setup") $ do
    entry $ liftIvory_ $ do
      store failure false
      -- send an i2c command to setup sensors read
      req <- fmap constRef $ local $ istruct
        [ tx_addr .= ival i2caddr
        , tx_buf  .= iarray [ ival (fromIntegral (regAddr OutXH)) ]
        , tx_len  .= ival 1
        , rx_len  .= ival 0
        ]
      emit_ req_emitter req
    on res_evt $ \res -> do
      liftIvory_ $ checki2csuccess res
      goto readPerform

  readPerform <- stateNamed (named "read_perform") $ do
    entry $ liftIvory_ $ do
      req <- fmap constRef $ local $ istruct
        [ tx_addr .= ival i2caddr
        , tx_buf  .= iarray []
        , tx_len  .= ival 0
        , rx_len  .= ival 6
        ]
      emit_ req_emitter req
    on res_evt $ \res -> do
      liftIvory_ $ do
        checki2csuccess res
        payloadu16 res 0 1 >>= store (value ! 0) -- XH, XL
        payloadu16 res 2 3 >>= store (value ! 2) -- ZH, ZL
        payloadu16 res 4 5 >>= store (value ! 1) -- YH, YL
      goto next

  return readSetup
  where
  named n = "hmc8553l_" ++ n
  checki2csuccess :: ConstRef s (Struct "i2c_transaction_result") -> Ivory eff ()
  checki2csuccess res = do
    r <- deref (res ~> resultcode)
    when (r >? 0) (store failure true)

  payloadu16 :: ConstRef s (Struct "i2c_transaction_result")
             -> Ix 128 -> Ix 128 -> Ivory eff Uint16
  payloadu16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ ((safeCast hi) * 256) + safeCast lo

