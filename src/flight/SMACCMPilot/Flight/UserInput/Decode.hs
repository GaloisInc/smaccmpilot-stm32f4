{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.UserInput.Decode where

import Prelude hiding (last)
import Control.Monad hiding (when, unless)

import Ivory.Language
import Ivory.Stdlib

-- needed because the ivory quasiquoter cannot handle qualified names
import SMACCMPilot.Flight.Types.FlightModeData (FlightMode)

import qualified SMACCMPilot.Flight.Types.Armed          as A
import qualified SMACCMPilot.Flight.Types.UserInput      as I
import qualified SMACCMPilot.Flight.Types.FlightMode     as FM
import qualified SMACCMPilot.Flight.Types.FlightModeData as FM

--------------------------------------------------------------------------------

userInputDecodeModule :: Module
userInputDecodeModule = package "userinput_decode" $ do
  depend I.userInputTypeModule
  depend FM.flightModeTypeModule
  defStruct (Proxy :: Proxy "flightmode_state")
  defStruct (Proxy :: Proxy "arming_state")
  incl userInputDecode
  incl userInputFailsafe
  incl userInputFilter
  incl deadManSwitch
  incl armingStatemachine
  incl modeStatemachine
  private $ incl scale_proc

-- | State of the arming state machine.  This is a separate type from
-- 'ArmedMode' because we need an intermediate "arming" state.
armStateDisarmed, armStateArming, armStateArmed :: Uint8
armStateDisarmed = 0
armStateArming   = 1
armStateArmed    = 2

[ivory|
struct flightmode_state
  { fm_last_mode         :: Stored FlightMode
  ; fm_last_mode_time    :: Stored Uint32
  }

struct arming_state
  { arm_state         :: Stored Uint8
  ; arm_state_time    :: Stored Uint32
  }

|]

--------------------------------------------------------------------------------


mode_ppm_map :: [(FM.FlightMode, (I.PPM, I.PPM))]    -- on ER9X this should be:
mode_ppm_map = [(FM.flightModeAuto,      (minBound, 1300))  -- AUX 3 up
               ,(FM.flightModeAltHold,   (1301, 1700))      -- AUX 3 center
               ,(FM.flightModeStabilize, (1701, maxBound)) -- AUX 3 down
               ]

scale_rpyt :: I.PPM -> Ivory eff IFloat
scale_rpyt input = call scale_proc I.ppmCenter 500 (-1.0) 1.0 input

scale_proc :: Def ('[I.PPM, Uint16, IFloat, IFloat, I.PPM] :-> IFloat)
scale_proc = proc "userinput_scale" $ \center range outmin outmax input ->
  requires (    (range /=? 0)
            .&& (input >=? minBound)
            .&& (input <=? maxBound)
           )
  $ body $ do
    let centered = safeCast input - safeCast center
    let ranged = centered / safeCast range
    ifte_ (ranged <? outmin)
      (ret outmin)
      (ifte_ (ranged >? outmax)
        (ret outmax)
        (ret ranged))

userInputDecode :: Def ('[ Ref s0 (Array 8 (Stored I.PPM))
                         , Ref s1 (Struct "userinput_result")
                         , Uint32 ] :-> ())
userInputDecode = proc "userinput_decode" $ \ppms ui now ->
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Ix 8
                  -> Label "userinput_result" (Stored IFloat)
                  -> Ivory eff ()
      chtransform ix ofield = do
        ppm <- deref (ppms ! (ix :: Ix 8))
        v   <- scale_rpyt ppm
        store (ui ~> ofield) v
  chtransform 0 I.roll
  chtransform 1 I.pitch
  chtransform 2 I.throttle
  chtransform 3 I.yaw
  store (ui ~> I.time) now
  retVoid

-- | Run the arming state machine, returning true if an arming event
-- has occured, writing the new arming state to the output reference.
armingStatemachine :: Def ('[ Ref s0 (I.PPMs)
                            , Ref s1 (Struct "arming_state")
                            , Ref s2 (Stored A.ArmedMode)
                            , Uint32
                            ] :-> IBool)
armingStatemachine = proc "armingStatemachine" $ \ppms state out now -> body $ do
  throttle_stick  <- deref (ppms ! (2 :: Ix 8))
  rudder_stick    <- deref (ppms ! (3 :: Ix 8))
  dmswitch        <- call deadManSwitch (constRef ppms)
  let hystresis    = 500
  let stick_in_pos = (throttle_stick <? 1050) .&& (rudder_stick >? 1900)

  -- If the switch is off or the stick is not in position, reset
  -- the state machine and return "no event".
  when (iNot dmswitch .|| iNot stick_in_pos) $ do
    store (state ~> arm_state) armStateDisarmed
    store (state ~> arm_state_time) now
    ret false

  -- The switch is on and the stick is in position.
  ast <- deref (state ~> arm_state)
  cond_
    [ ast ==? armStateDisarmed ==> do
      -- Record the start time and go to state "arming".
      store (state ~> arm_state_time) now
      store (state ~> arm_state) armStateArming
    , ast ==? armStateArming ==> do
      -- If enough time has elapsed in the "arming" state, go
      -- to start "armed" and send a positive arming event.
      -- Otherwise, do nothing.
      astime <- deref (state ~> arm_state_time)
      when (now - astime >? hystresis) $ do
        store (state ~> arm_state) armStateArmed
        store out A.as_ARMED
        ret true
    ] -- do nothing when already in the "armed" state

  ret false

-- | Debouncing state machine for the flight mode switch.
modeStatemachine :: Def ('[ Ref s0 I.PPMs
                          , Ref s1 (Struct "flightmode_state")
                          , Ref s2 (Struct "flightmode")
                          , Uint32 -- now
                          ] :-> IBool)
modeStatemachine = proc "modeStatemachine" $ \ppms state out now -> body $ do
  ppm <- deref (ppms ! (4 :: Ix 8))
  let mode = mode_from_ppm ppm
  last_mode <- deref (state ~> fm_last_mode)

  -- XXX TODO: debounce
  when (mode /=? last_mode) $ do
    store (out ~> FM.mode) mode
    store (out ~> FM.time) now
    store (state ~> fm_last_mode) mode
    store (state ~> fm_last_mode_time) now
    ret true

  ret false
  where
  mode_from_ppm :: I.PPM -> FM.FlightMode
  mode_from_ppm ppm = foldr (match_mode_map ppm) FM.flightModeStabilize mode_ppm_map
  match_mode_map ppm (mode, (min_ppm, max_ppm)) def =
    ((ppm >=? min_ppm) .&& (ppm <=? max_ppm)) ? (mode, def)

-- | Is Channel 6 (switch 2) depressed?  True means yes: ARMing ok.
deadManSwitch :: Def ('[ConstRef s I.PPMs] :-> IBool)
deadManSwitch = proc "deadManSwitch" $ \ppms -> body $ do
  ch5_switch <- deref (ppms ! (5 :: Ix 8))
  ret (ch5_switch >=? I.ppmHigh)

-- Filter for timeouts.
userInputFailsafe :: Def ('[ Ref s (Struct "userinput_result")
                           , Uint32 ] :-> ())
userInputFailsafe = proc "userinput_failsafe" $ \capt now ->
  requires (checkStored (capt ~> I.time) (\t -> now >=? t))
  $ body $ do
    last <- deref ( capt ~> I.time )
    let dt = now - last
    when (dt >? 150) $ do
       store (capt ~> I.throttle) (-1)
       store (capt ~> I.yaw)      0
       store (capt ~> I.pitch)    0
       store (capt ~> I.roll)     0
    retVoid

-- Filter for bad/noisy PPM signals.
userInputFilter :: Def ('[ConstRef s I.PPMs] :-> IBool)
userInputFilter = proc "userinput_filter" $ \ppms -> body $ do
  let go b ix = do
        ppm <- deref (ppms ! toIx (fromInteger ix :: Sint32))
        return $ b .&& ppm >=? minBound .&& ppm <=? maxBound

  ret =<< foldM go true [0..5 :: Integer]

