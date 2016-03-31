{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.ADC
  ( adcTower
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Hardware.Platforms (ADC(..))
import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32.Peripheral.GPIOF4

adcTower :: (e -> ADC)
         -> ChanInput ('Stored IFloat)
         -> Tower e ()
adcTower toadc output = do
  adc <- fmap toadc getEnv
  p   <- period (Milliseconds 1000)
  aux adc p
  where
  aux ADC{..} p = monitor "adc" $ do

    handler systemInit "adc_init" $ callback $ const $ do
      pinEnable  adc_pin
      pinSetMode adc_pin gpio_mode_analog
      adcInit adc_periph adc_12bit false
      adcStartConversion adc_periph adc_chan

    handler p "adc_periodic" $ do
      e <- emitter output 1
      callback $ const $ do
        conv <- adcGetConversion adc_periph
        emitV e (adc_cal (safeCast conv))
        adcStartConversion adc_periph adc_chan


