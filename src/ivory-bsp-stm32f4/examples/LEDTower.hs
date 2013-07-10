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

-- | LED Controller: Given a set of GPIO pins and a control channel of booleans,
--   setup the pin hardware, and turn the pins on when the control channel is
--   true.
ledController :: (SingI n) => [GPIOPin] -> ChannelSink n (Stored IBool) -> Task ()
ledController pins outputSink = do
  -- Bring the channel into scope for this Task
  rxer <- withChannelReceiver outputSink "outputSink"
  -- Bookkeeping: this task uses Ivory.HW.Module.hw_moduledef
  taskModuleDef $ const hw_moduledef
  -- Task Body is parameterized on receiver
  taskBody (ledTaskBody rxer)
  where
  -- Task body takes receiver and schedule
  ledTaskBody rxer sch = do
    -- First, setup hardware
    mapM_ ledSetupPin pins
    -- Then, make an event loop with one handler.
    eventLoop sch $ onChannelV rxer $ \out-> do
      -- Turn pins on or off according to event value
      ifte_ out
        (mapM_ ledOn pins)
        (mapM_ ledOff pins)

-- | Blink task: Given a period and a channel source, output an alternating
--   stream of true / false on each period.
blink :: (SingI n) => Integer -> ChannelSource n (Stored IBool) -> Task ()
blink per outSource = do
  -- Bring the emitter into scope for this Task
  outEmitter <- withChannelEmitter outSource "output"
  -- Declare a period for this Task
  t <- withPeriod per
  taskBody $ \sch -> do
    -- Make an event loop with a handler for the period declared above
    eventLoop sch $ onTimer t $ \time ->
      -- Emit boolean value which will alternate each period.
      emitV_ outEmitter ((time .% 2*p) <? p)
  where p = fromIntegral per :: Uint32

blinkApp :: Integer -> [GPIOPin] -> Tower ()
blinkApp period pins = do
  (src_led, sink_led) <- channel
  task "blink"  $ blink         period src_led
  task "ledhw"  $ ledController pins   sink_led

