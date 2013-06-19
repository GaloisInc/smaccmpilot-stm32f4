{-# LANGUAGE DataKinds #-}

module LEDTower where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO

ledSetupPin :: GPIOPin -> Ivory eff ()
ledSetupPin pin = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
  pinSetMode pin gpio_mode_analog

ledOn :: GPIOPin -> Ivory eff ()
ledOn pin = do
  pinClear pin
  pinSetMode pin gpio_mode_output

ledOff :: GPIOPin -> Ivory eff ()
ledOff pin = pinSetMode pin gpio_mode_analog

------------------------------

blink :: Integer -> ChannelSource (Stored IBool) -> Task ()
blink per outputSource = do
  outputEmitter <- withChannelEmitter outputSource "output"
  p <- withPeriod per
  taskBody $ \sch -> do
    out <- local (ival false)
    eventLoop sch $ onTimer p $ \time -> do
      store out ((time .% (fromIntegral (2*per))) <? (fromIntegral per))
      emit_ sch outputEmitter (constRef out)

ledController :: [GPIOPin] -> ChannelSink (Stored IBool) -> Task ()
ledController pins outputSink = do
  rxer <- withChannelReceiver outputSink "outputSink"
  p <- withPeriod 250
  taskModuleDef $ const hw_moduledef
  taskBody $ \sch -> do
    mapM_ ledSetupPin pins
    eventLoop sch $ onChannel rxer $ \stateref -> do
      out <- deref stateref
      ifte_ out
        (mapM_ ledOn pins)
        (mapM_ ledOff pins)

blinkApp :: Integer -> [GPIOPin] -> Tower ()
blinkApp period pins = do
  (src_led, sink_led) <- channel
  task "blink"  $ blink         period src_led
  task "ledhw"  $ ledController pins   sink_led

