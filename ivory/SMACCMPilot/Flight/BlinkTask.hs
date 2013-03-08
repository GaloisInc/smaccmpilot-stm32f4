{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.BlinkTask where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS

import Smaccm.Stm32f4.GPIO

import SMACCMPilot.Driver.Gpio
import SMACCMPilot.Util.IvoryHelpers
import SMACCMPilot.Util.Periodic

blinkTask :: MemArea (Struct "pin")
          -> Sink (Stored Uint8)
          -> String -> Task
blinkTask mempin s uniquename =
  withSink "blinkModeSink" s $ \blinkModeSink ->
  let tDef = proc ("blinkTaskDef" ++ uniquename) $ body $ do
        pin <- addrOf mempin 
        call_ pin_enable     pin
        call_ pin_set_otype  pin pinTypePushPull
        call_ pin_set_ospeed pin pinSpeed2Mhz
        call_ pin_set_pupd   pin pinPupdNone
        call_ pin_reset      pin
        call_ pin_set_mode   pin pinModeOutput
        state    <- local (ival (0::Uint8))
        phase    <- local (ival (0::Uint8))
        periodic 125 $ do
          sink blinkModeSink state
          output <- findBlinkOutput state phase
          ifte output
            (call_ pin_set  pin)
            (call_ pin_reset pin)
          nextPhase 8 phase

      findBlinkOutput :: (Ref s1 (Stored Uint8))
        -> (Ref s2 (Stored Uint8)) -> Ivory s () IBool
      findBlinkOutput rMode rPhase = do
        mode <- deref rMode
        phase <- deref rPhase
        return $ findBlinkOutput' mode phase

      nextPhase :: Uint8 -> (Ref s1 (Stored Uint8)) -> Ivory s () ()
      nextPhase highest r = do
          phase <- deref r
          ifte (phase >? (highest - 1))
            (store r 0)
            (store r (phase + 1))

      mDefs = do
        depend OS.taskModule
        depend gpioModule
        inclHeader "userinput_capture"
        incl tDef

  in task tDef mDefs


findBlinkOutput' :: Uint8 -> Uint8 -> IBool
findBlinkOutput' state phase = switchState
  where
  switchState = foldl aux false [0..6]
    where aux res s = (state ==? (fromIntegral s)) ? (switchPhase s, res)
  switchPhase s = foldl aux false [0..7]
    where aux res n = (phase ==? (fromIntegral n)) ? (((t !! s) !! n), res)
  t = [ [ false, false, false, false, false, false, false, false ]  -- off
      , [  true,  true,  true,  true,  true,  true,  true,  true ]  -- on
      , [ false, false,  true,  true,  true,  true,  true,  true ]  -- blinkslow
      , [ false,  true,  true,  true, false,  true,  true,  true ]  -- blinkfast
      , [ false, false, false, false, false, false, false,  true ]  -- pulseslow
      , [ false, false, false,  true, false, false, false,  true ]  -- pulsefast
      , [ false,  true, false,  true, false,  true, false,  true ] ]-- pulsexfst


