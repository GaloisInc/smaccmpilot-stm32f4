{-# LANGUAGE DataKinds #-}

module BSP.Tests.LED.Blink where

import Ivory.Language
import Ivory.Tower
import Ivory.HW.Module

import BSP.Tests.LED
------------------------------

-- | LED Controller: Given a set of leds and a control channel of booleans,
--   setup the pin hardware, and turn the leds on when the control channel is
--   true.
ledController :: [LED] -> ChannelSink (Stored IBool) -> Task p ()
ledController leds rxer = do
  -- Bookkeeping: this task uses Ivory.HW.Module.hw_moduledef
  taskModuleDef $ hw_moduledef
  -- Setup hardware before running any event handlers
  taskInit $
    mapM_ ledSetup leds
  -- Run a callback on each message posted to the channel
  rxevt <- withChannelEvent rxer "output"
  handle rxevt "output" $ \outref -> do
    out <- deref outref
    -- Turn pins on or off according to event value
    ifte_ out
      (mapM_ ledOn  leds)
      (mapM_ ledOff leds)

-- | Blink task: Given a period and a channel source, output an alternating
--   stream of true / false on each period.
blinkTask :: Integer -> ChannelSource (Stored IBool) -> Task p ()
blinkTask per outSource = do
  -- Bring the emitter into scope for this Task
  outEmitter <- withChannelEmitter outSource "output"
  -- Declare a period for this Task
  perevt <- withPeriodicEvent (Milliseconds per)
  handle perevt "per" $ \timeref -> do
    time <- deref timeref
    -- Emit boolean value which will alternate each period.
    emitV_ outEmitter ((toIMilliseconds time) .% (2*p) <? p)
  where p = fromIntegral per

blink :: Integer -> [LED] -> Tower p ()
blink period pins = do
  (src_led, sink_led) <- channel
  task "blink"  $ blinkTask     period src_led
  task "ledhw"  $ ledController pins   sink_led

