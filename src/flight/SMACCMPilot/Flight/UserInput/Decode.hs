{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.UserInput.Decode where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.Armed          as A
import           SMACCMPilot.Flight.Types.Armed(ArmedMode)
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
  incl setFlightMode
  incl deadManSwitch
  incl armingStatemachine
  private $ incl scale_proc

-- | State of the arming state machine.  This is a separate type from
-- 'ArmedMode' because we need an intermediate "arming" state.
armStateDisarmed, armStateArming, armStateArmed :: Uint8
armStateDisarmed = 0
armStateArming   = 1
armStateArmed    = 2

[ivory|
struct flightmode_state
  { last_modepwm      :: Stored Uint16
  ; last_modepwm_time :: Stored Uint32
  ; valid_modepwm     :: Stored Uint16
  }
struct arming_state
  { arm_state         :: Stored Uint8
  ; arm_state_time    :: Stored Uint32
  }

|]

mode_pwm_map :: [(FM.FlightMode, (Uint16, Uint16))]    -- on ER9X this should be:
mode_pwm_map = [(FM.flightModeAuto,      (900, 1300))  -- AUX 3 up
               ,(FM.flightModeAltHold,   (1301, 1700)) -- AUX 3 center
               ,(FM.flightModeStabilize, (1701, 2100)) -- AUX 3 down
               ]

scale_rpyt :: Uint16 -> Ivory eff IFloat
scale_rpyt input = call scale_proc 1500 500 (-1.0) 1.0 input

scale_proc :: Def ('[Uint16, Uint16, IFloat, IFloat, Uint16] :-> IFloat)
scale_proc = proc "userinput_scale" $ \center range outmin outmax input ->
  requires (    (range /=? 0)
            .&& (input >=? 900)
            .&& (input <=? 2100)
           )
  $ body $ do
    let centered = safeCast input - safeCast center
    let ranged = centered / safeCast range
    ifte_ (ranged <? outmin)
      (ret outmin)
      (ifte_ (ranged >? outmax)
        (ret outmax)
        (ret ranged))

userInputDecode :: Def ('[ Ref s0 (Array 8 (Stored Uint16))
                         , Ref s1 (Struct "userinput_result")
                         , Uint32 ] :-> ())
userInputDecode = proc "userinput_decode" $ \pwms ui now ->
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Ix 8
                  -> Label "userinput_result" (Stored IFloat)
                  -> Ivory eff ()
      chtransform ix ofield = do
        pwm <- deref (pwms ! (ix :: Ix 8))
        v   <- scale_rpyt pwm
        store (ui ~> ofield) v
  chtransform 0 I.roll
  chtransform 1 I.pitch
  chtransform 2 I.throttle
  chtransform 3 I.yaw
  store (ui ~> I.time) now
  retVoid

setFlightMode :: Def ('[ Ref s0 (Array 8 (Stored Uint16))
                       , Ref s1 (Struct "flightmode_state")
                       , Ref s2 (Struct "arming_state")
                       , Ref s3 (Struct "flightmode")
                       , Uint32
                       ] :-> ())
setFlightMode = proc "set_flight_mode" $ \pwms fm_state arming_state fm now ->
  requires (checkStored (arming_state ~> arm_state_time) (\ast -> now >=? ast))
  $ body $ do
  mode  <- mode_statemachine pwms fm_state now
  store (fm ~> FM.mode)  mode
  store (fm ~> FM.time)  now
  retVoid

-- | Run the arming state machine, returning true if an arming event
-- has occured, writing the new arming state to the output reference.
armingStatemachine :: Def ('[ Ref s0 (Array 8 (Stored Uint16))
                            , Ref s1 (Struct "arming_state")
                            , Ref s2 (Stored A.ArmedMode)
                            , Uint32
                            ] :-> IBool)
armingStatemachine = proc "armingStatemachine" $ \pwms state out now -> body $ do
  throttle_stick  <- deref (pwms ! (2 :: Ix 8))
  rudder_stick    <- deref (pwms ! (3 :: Ix 8))
  sw1             <- call deadManSwitch (constRef pwms)
  let hystresis    = 500
  let stick_in_pos = (throttle_stick <? 1050) .&& (rudder_stick >? 1900)

  -- If the switch is off or the stick is not in position, reset
  -- the state machine and return "no event".
  when (iNot sw1 .|| iNot stick_in_pos) $ do
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

mode_statemachine :: (Ref s1 (Array 8 (Stored Uint16)))
                  -> (Ref s2 (Struct "flightmode_state"))
                  -> Uint32
                  -> Ivory eff FM.FlightMode
mode_statemachine pwms state now = do
  mode_input_current <- deref (pwms ! (4 :: Ix 8))
  mode_input_prev    <- deref (state ~> last_modepwm)
  prev_time          <- deref (state ~> last_modepwm_time)
  let pwmtolerance = 10
  let latchtime    = 250
  ifte_ (magnitude mode_input_current mode_input_prev >? pwmtolerance)
    (when (now - prev_time >? latchtime)
      (newmode mode_input_current))
    (reset_input mode_input_current)
  m <- deref (state ~> valid_modepwm)
  return $ mode_from_pwm m
  where
  reset_input m = do
    store (state ~> last_modepwm) m
    store (state ~> last_modepwm_time) now
  newmode m = do
    reset_input m
    store (state ~> valid_modepwm) m
  mode_from_pwm :: Uint16 -> FM.FlightMode
  mode_from_pwm pwm = foldr matchmodemap FM.flightModeStabilize mode_pwm_map
    -- Build up a series of conditional checks using fold. Default to
    -- mode_STABILIZE if none found.
    where
    matchmodemap (mode, (minpwm, maxpwm)) dflt =
      ((pwm >=? minpwm) .&& (pwm <=? maxpwm)) ? (mode, dflt)

  magnitude :: Uint16 -> Uint16 -> Uint16
  magnitude a b = castDefault
                $ abs $ (safeCast a :: Sint32) - (safeCast b)

-- | Is Channel 5 (switch 1) depressed?  True means yes: ARMing ok.
deadManSwitch :: Def ('[ConstRef s (Array 8 (Stored Uint16))] :-> IBool)
deadManSwitch = proc "deadManSwitch" $ \pwms -> body $ do
  ch5_switch <- deref (pwms ! (5 :: Ix 8))
  ret (ch5_switch >=? 1500)

userInputFailsafe :: Def ('[ Ref s0 (Struct "userinput_result")
                           , Uint32 ] :-> ())
userInputFailsafe = proc "userinput_failsafe" $ \capt now ->
  requires (checkStored (capt ~> I.time) (\t -> now >=? t))
  $ body $ do
    last <- deref ( capt ~> I.time )
    let dt = now - last
    when (dt >? 150) $ do
       store (capt ~> I.throttle) 0
       store (capt ~> I.yaw)      0
       store (capt ~> I.pitch)    0
       store (capt ~> I.roll)     0
    retVoid

