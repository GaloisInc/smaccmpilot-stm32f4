{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.BlinkTask
  ( blinkTask
  ) where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.HWF4.GPIO

import SMACCMPilot.Util.IvoryHelpers

import qualified SMACCMPilot.Flight.Types.FlightMode as FM

blinkTask :: MemArea (Struct "pin")
          -> DataSink (Struct "flightmode")
          -> Task ()
blinkTask mempin s =  do
  fmReader <- withDataReader s "flightmode"
  p <- withPeriod 125
  taskBody $ \sch -> do
    pin <- addrOf mempin
    call_ pin_enable     pin
    call_ pin_set_otype  pin pinTypePushPull
    call_ pin_set_ospeed pin pinSpeed2Mhz
    call_ pin_set_pupd   pin pinPupdNone
    call_ pin_reset      pin
    call_ pin_set_mode   pin pinModeOutput
    flightMode <- local (istruct [])
    s_phase    <- local (ival (0::Uint8))
    eventLoop sch $ onTimer p $ \_now -> do
      readData sch fmReader flightMode
      bmode  <- flightModeToBlinkMode flightMode
      phase  <- nextPhase 8 s_phase
      output <- blinkOutput bmode phase
      ifte output
        (call_ pin_reset pin) -- relay LEDs are active low.
        (call_ pin_set   pin)
  taskModuleDef $ \_sch -> do
    depend gpioModule

nextPhase :: Uint8 -> (Ref s1 (Stored Uint8)) -> Ivory eff Uint8
nextPhase highest r = do
    phase <- deref r
    next <- assign (phase + 1)
    ifte (next >=? highest)
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
    , ( disarmed .&& loiter   , 6 )
    , ( armed    .&& stabilize, 4 )
    , ( armed    .&& althold  , 5 )
    , ( armed    .&& loiter   , 1 ) ]
    where
    disarmed  = iNot armed
    stabilize = mode ==? FM.flightModeStabilize
    althold   = mode ==? FM.flightModeAltHold
    loiter    = mode ==? FM.flightModeLoiter


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


