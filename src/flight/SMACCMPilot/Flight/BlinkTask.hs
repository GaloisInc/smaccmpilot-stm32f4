{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.BlinkTask
  ( blinkTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module (hw_moduledef)

import Ivory.BSP.STM32F4.GPIO

import qualified SMACCMPilot.Flight.Types.ArmedMode      as A
import qualified SMACCMPilot.Flight.Types.ControlLaw     as CL

blinkTask :: [ GPIOPin ]
          -> DataSink (Struct "control_law")
          -> Task p ()
blinkTask pins cls = do
  lawReader <- withDataReader cls "controllaw"
  taskInit $ mapM_ pinInit pins

  s_phase    <- taskLocal "phase"

  onPeriod 125 $ \_now -> do
    ctllaw <- local (istruct [])
    readData lawReader ctllaw
    bmode  <- lawToBlinkMode ctllaw
    phase  <- nextPhase 8 s_phase
    output <- blinkOutput bmode phase
    ifte_ output
      (mapM_ lightOn  pins)
      (mapM_ lightOff pins)
  taskModuleDef $ do
    hw_moduledef

pinInit :: GPIOPin -> Ivory eff ()
pinInit pin = do
    pinEnable        pin
    pinSetOutputType pin gpio_outputtype_pushpull
    pinSetSpeed      pin gpio_speed_2mhz
    pinSetPUPD       pin gpio_pupd_none
    pinClear         pin
    pinSetMode       pin gpio_mode_output

-- relay LEDs are active low.
lightOn :: GPIOPin -> Ivory eff ()
lightOn p = pinClear p

lightOff :: GPIOPin -> Ivory eff ()
lightOff p = pinSet p

nextPhase :: Uint8 -> (Ref s1 (Stored Uint8)) -> Ivory eff Uint8
nextPhase highest r = do
    phase <- deref r
    next <- assign (phase + 1)
    ifte_ (next >=? highest)
      (store r 0)
      (store r next)
    return phase


lawToBlinkMode :: (GetAlloc eff ~ Scope cs)
               => Ref s (Struct "control_law")
               -> Ivory eff Uint8
lawToBlinkMode law = do
  armed <- deref (law ~> CL.armed_mode)
  cond
    [ armed ==? A.safe     ==> return pulse_slow
    , armed ==? A.disarmed ==> return pulse_fast
    , armed ==? A.armed    ==> return blink_fast
    ]

blink_off :: Uint8
blink_off = 0
blink_on :: Uint8
blink_on = 1
blink_slow :: Uint8
blink_slow = 2
blink_fast :: Uint8
blink_fast = 3
pulse_slow :: Uint8
pulse_slow = 4
pulse_fast :: Uint8
pulse_fast = 5
pulse_xfast :: Uint8
pulse_xfast = 6

blinkOutput :: Uint8 -> Uint8 -> Ivory eff IBool
blinkOutput state phase = return switchState
  where
  switchState = foldl aux false [0..6]
    where aux res s = (state ==? (fromIntegral s)) ? (switchPhase s, res)
  switchPhase s = foldl aux false [0..7]
    where aux res n = (phase ==? (fromIntegral n)) ? (((t !! s) !! n), res)
  t = [ [ false, false, false, false, false, false, false, false ]  -- 0 off
      , [  true,  true,  true,  true,  true,  true,  true,  true ]  -- 1 on
      , [ false, false,  true,  true,  true,  true,  true,  true ]  -- 2 blinkslow
      , [ false,  true,  true,  true, false,  true,  true,  true ]  -- 3 blinkfast
      , [ false, false, false, false, false, false, false,  true ]  -- 4 pulseslow
      , [ false, false, false,  true, false, false, false,  true ]  -- 5 pulsefast
      , [ false,  true, false,  true, false,  true, false,  true ] ]-- 6 pulsexfst


