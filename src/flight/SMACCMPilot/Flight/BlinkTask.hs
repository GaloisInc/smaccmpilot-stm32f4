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
import qualified SMACCMPilot.Flight.Types.CommsecStatus  as CS

blinkTask :: [ GPIOPin ]
          -> DataSink (Struct "control_law")
          -> DataSink (Stored CS.CommsecStatus)
          -> Task p ()
blinkTask pins cls css = do
  lawReader <- withDataReader cls "controllaw"
  comStatRr <- withDataReader css "commstatus"
  taskInit $ mapM_ pinInit pins

  s_phase    <- taskLocal "phase"

  onPeriod 125 $ \_now -> do
    ctllaw <- local (istruct [])
    readData lawReader ctllaw
    commStatRef <- local izero
    readData comStatRr commStatRef
    commStat <- deref commStatRef

    bmode  <- lawToBlinkMode ctllaw commStat
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
               -> CS.CommsecStatus
               -> Ivory eff Uint8
lawToBlinkMode law cstat = do
  armed <- deref (law ~> CL.armed_mode)
  cond
    [ armed ==? A.safe     ==> return shortduration_longperiod
    , cstat ==? CS.alarm   ==> return longduration_shortperiod
    , armed ==? A.disarmed .|| armed ==? A.armed ==>
        return shortduration_shortperiod
    ]

longduration_shortperiod :: Uint8
longduration_shortperiod = 0
shortduration_longperiod :: Uint8
shortduration_longperiod = 1
shortduration_shortperiod :: Uint8
shortduration_shortperiod = 2

blinkOutput :: Uint8 -> Uint8 -> Ivory eff IBool
blinkOutput state phase = return (iNot switchState) -- XXX HACK FOR INVERTED EXTERNAL LEDS
  where
  switchState = foldl aux false [0..2]
    where aux res s = (state ==? (fromIntegral s)) ? (switchPhase s, res)
  switchPhase s = foldl aux false [0..7]
    where aux res n = (phase ==? (fromIntegral n)) ? (((t !! s) !! n), res)
  t = [ [  true,  true,  true, false,  true,  true,  true, false ]  -- 0 longduration_shortperiod
      , [  true, false, false, false, false, false, false, false ]  -- 1 shortduration_longperiod
      , [  true, false, false, false,  true, false, false, false ]] -- 2 shortduration_shortperiod


