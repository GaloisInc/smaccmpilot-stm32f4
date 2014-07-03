
module BSP.Tests.LED where

import Ivory.Language

import Ivory.BSP.STM32F405.GPIO

data LEDPolarity = ActiveHigh | ActiveLow
data LED = LED GPIOPin LEDPolarity

ledSetup :: LED -> Ivory eff ()
ledSetup led@(LED pin _polarity) = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
  ledOff led

ledOn :: LED -> Ivory eff ()
ledOn (LED pin ActiveHigh) = pinHigh pin
ledOn (LED pin ActiveLow)  = pinLow  pin

ledOff :: LED -> Ivory eff ()
ledOff (LED pin _) = pinHiZ pin

pinLow :: GPIOPin -> Ivory eff ()
pinLow pin = do
  pinClear pin
  pinSetMode pin gpio_mode_output

pinHigh :: GPIOPin -> Ivory eff ()
pinHigh pin = do
  pinSet pin
  pinSetMode pin gpio_mode_output

pinHiZ :: GPIOPin -> Ivory eff ()
pinHiZ pin = pinSetMode pin gpio_mode_analog

