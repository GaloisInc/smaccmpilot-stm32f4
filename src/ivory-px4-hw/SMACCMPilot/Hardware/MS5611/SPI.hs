{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MS5611.SPI where

import SMACCMPilot.Hardware.MS5611.Manager
import SMACCMPilot.Hardware.MS5611.Regs

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Driver.SPI

ms5611_SPI :: MS5611Impl (Struct "spi_transaction_request")
                         (Struct "spi_transaction_result")
                         SPIDeviceHandle
ms5611_SPI = MS5611Impl
  { ms5611_command_req = \h cmd -> fmap constRef $ local $ istruct
      [ tx_device .= ival h
      , tx_buf .= iarray [ ival (commandVal cmd) ]
      , tx_len .= ival 1
      ]
  , ms5611_prom_fetch_req = \h -> fmap constRef $ local $ istruct
      [ tx_device .= ival h
      , tx_buf    .= iarray (repeat (ival 0))
      , tx_len    .= ival 2
      ] -- XXX PROBABLY WRONG - PROM FETCHES SUPPOSED TO BE FUSED WITH CMD
  , ms5611_adc_fetch_req = \h -> fmap constRef $ local $ istruct
      [ tx_device .= ival h
      , tx_buf    .= iarray (repeat (ival 0))
      , tx_len    .= ival 3
      ] -- XXX PROBABLY WRONG - ADC FETCHES SUPPOSED TO BE FUSED WITH CMD
  , ms5611_res_code = \r -> deref (r ~> resultcode)
  , ms5611_res_prom = \r -> do
      h <- deref ((r ~> rx_buf) ! 0) -- XXX THESE INDEXES MIGHT BE WRONG
      l <- deref ((r ~> rx_buf) ! 1)
      assign (u16_from_2_bytes h l)
  , ms5611_res_sample = \r -> do
      h <- deref ((r ~> rx_buf) ! 0) -- XXX THESE INDEXES MIGHT BE WRONG
      m <- deref ((r ~> rx_buf) ! 1)
      l <- deref ((r ~> rx_buf) ! 2)
      assign (u32_from_3_bytes h m l)
  }


ms5611SPISensorManager :: ChanInput  (Struct "spi_transaction_request")
                       -> ChanOutput (Struct "spi_transaction_result")
                       -> ChanOutput (Stored ITime)
                       -> ChanInput  (Struct "ms5611_measurement")
                       -> SPIDeviceHandle
                       -> Tower e ()
ms5611SPISensorManager = ms5611SensorManager ms5611_SPI
