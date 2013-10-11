{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.BlinkTask
  ( blinkTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW.Module (hw_moduledef)

import Ivory.BSP.STM32F4.GPIO

import qualified SMACCMPilot.Flight.Types.FlightMode as FM

blinkTask :: [ GPIOPin ]
          -> DataSink (Struct "flightmode")
          -> Task p ()
blinkTask pins s = do
  fmReader <- withDataReader s "flightmode"
  taskInit $ mapM_ pinInit pins

  flightMode <- taskLocal "flightmode"
  s_phase    <- taskLocal "phase"

  onPeriod 125 $ \_now -> do
    readData fmReader flightMode
    bmode  <- flightModeToBlinkMode flightMode
    phase  <- nextPhase 8 s_phase
    output <- blinkOutput bmode phase
    ifte_ output
      (mapM_ pinOn pins)
      (mapM_ pinOff pins)
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
pinOn :: GPIOPin -> Ivory eff ()
pinOn p = pinClear p

pinOff :: GPIOPin -> Ivory eff ()
pinOff p = pinSet p

nextPhase :: Uint8 -> (Ref s1 (Stored Uint8)) -> Ivory eff Uint8
nextPhase highest r = do
    phase <- deref r
    next <- assign (phase + 1)
    ifte_ (next >=? highest)
      (store r 0)
      (store r next)
    return phase

flightModeToBlinkMode :: Ref s1 (Struct "flightmode") -> Ivory eff Uint8
flightModeToBlinkMode fmRef = do
  mode  <- (fmRef ~>* FM.mode)
  armed <- (fmRef ~>* FM.armed)
  return $ foldr cond 0 (tbl armed mode)
  where
  cond (c, res) k = c ? (res, k) 
  tbl :: IBool -> Uint8 -> [(IBool, Uint8)]
  tbl armed mode =
    [ ( disarmed .&& stabilize, 2 )
    , ( disarmed .&& althold  , 3 )
    , ( disarmed .&& auto     , 6 )
    , ( armed    .&& stabilize, 4 )
    , ( armed    .&& althold  , 5 )
    , ( armed    .&& auto     , 1 ) ]
    where
    disarmed  = iNot armed
    stabilize = mode ==? FM.flightModeStabilize
    althold   = mode ==? FM.flightModeAltHold
    auto      = mode ==? FM.flightModeAuto


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


