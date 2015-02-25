{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MS5611.I2C where

import SMACCMPilot.Hardware.MS5611.Manager
import SMACCMPilot.Hardware.MS5611.Regs

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C

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
