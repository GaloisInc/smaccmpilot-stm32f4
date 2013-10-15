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

[ivory|
struct flightmode_state
  { last_modepwm      :: Stored Uint16
  ; last_modepwm_time :: Stored Uint32
  ; valid_modepwm     :: Stored Uint16
  }
struct arming_state
  { arm_state         :: Stored ArmedMode
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
            .&& (input >=? 1000)
            .&& (input <=? 2000)
            .&& (input >=? center)
           )
  $ body $ do
    let centered = input - center
    let ranged = safeCast centered / safeCast range
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

-- | Run the arming state machine.  Return true if the system is armed.
armingStatemachine :: Def ('[ Ref s0 (Array 8 (Stored Uint16))
                            , Ref s1 (Struct "arming_state")
                            , Uint32
                            ] :-> IBool)
armingStatemachine = proc "armingStatemachine" $ \pwms state now -> body $ do
  throttle_stick <- deref (pwms ! (2 :: Ix 8))
  rudder_stick   <- deref (pwms ! (3 :: Ix 8))

  let set_arm_state :: A.ArmedMode -> Ivory eff ()
      set_arm_state newstate = do
        store (state ~> arm_state) newstate
        store (state ~> arm_state_time) now

  let do_disarm :: Ivory eff ()
      do_disarm = set_arm_state A.as_DISARMED

  let hystresis = 500

  let do_try_arming :: Ivory eff ()
      do_try_arming = do
        ast    <- state ~>* arm_state
        astime <- state ~>* arm_state_time
        let longEnough = now - astime >? hystresis
        (ifte_ (ast ==? A.as_DISARMED)
          (when longEnough (set_arm_state A.as_ARMING))
          (when (ast ==? A.as_ARMING)
            (when longEnough (set_arm_state A.as_ARMED))))

  let do_not_arming :: Ivory eff ()
      do_not_arming = do
        ast <- state ~>* arm_state
        when (ast ==? A.as_ARMING)
          (set_arm_state A.as_DISARMED)

  sw1 <- call deadManSwitch pwms

  ifte_ sw1
    (ifte_ ((throttle_stick <? 1050) .&& (rudder_stick >? 1900))
      do_try_arming
      do_not_arming)
    do_disarm
  newstate <- (state ~>* arm_state)
  ret (newstate ==? A.as_ARMED)

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

-- | Is Channel 5 (switch 1) depressed?  True means OK.
deadManSwitch :: Def ('[Ref s (Array 8 (Stored Uint16))] :-> IBool)
deadManSwitch = proc "deadManSwitch" $ \pwms -> body $ do
  ch5_switch <- deref (pwms ! (5 :: Ix 8))
  ret (ch5_switch >=? 1500)

userInputFailsafe :: Def ('[ Ref s1 (Struct "userinput_result")
                           , Ref s2 (Struct "flightmode")
                           , Ref s3 (Stored IBool)
                           , Uint32 ] :-> ())
userInputFailsafe = proc "userinput_failsafe" $ \capt fm armed now ->
  requires (checkStored (capt ~> I.time) (\t -> now >=? t))
  $ body $ do
    last <- deref ( capt ~> I.time )
    let dt = now - last
    when (dt >? 150) $ do
       store (capt ~> I.throttle) 0
       store (capt ~> I.yaw)      0
       store (capt ~> I.pitch)    0
       store (capt ~> I.roll)     0
       store armed false
    retVoid

