{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.UserInput.Decode where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput as I
import qualified SMACCMPilot.Flight.Types.FlightMode as FM

userInputDecodeModule :: Module
userInputDecodeModule = package "userinput_decode" $ do
  depend I.userInputTypeModule
  depend FM.flightModeTypeModule
  defStruct (Proxy :: Proxy "userinput_decode_state")
  incl userInputDecode
  incl userInputFailsafe
  private $ incl scale_proc

[ivory|
struct userinput_decode_state
  { last_modepwm      :: Stored Uint16
  ; last_modepwm_time :: Stored Uint32
  ; valid_modepwm     :: Stored Uint16
  ; arm_state         :: Stored Uint8
  ; arm_state_time    :: Stored Uint32
  }
|]

as_DISARMED, as_ARMING, as_ARMED :: Uint8
as_DISARMED = 0
as_ARMING   = 1
as_ARMED    = 2

mode_pwm_map :: [(Uint8, (Uint16, Uint16))]    -- on ER9X this should be:
mode_pwm_map = [(FM.flightModeLoiter,    (900, 1300))  -- AUX 3 up
               ,(FM.flightModeAltHold,   (1301, 1700)) -- AUX 3 center
               ,(FM.flightModeStabilize, (1701, 2100)) -- AUX 3 down
               ]

userInputDecode :: Def ('[ Ref s1 (Array 8 (Stored Uint16))
                         , Ref s2 (Struct "userinput_decode_state")
                         , Ref s3 (Struct "userinput_result")
                         , Ref s3 (Struct "flightmode")
                         , Uint32 ] :-> ())
userInputDecode = proc "userinput_decode" $ \pwms state ui fm now ->
    requires (checkStored (state ~> arm_state_time) (\ast -> now >=? ast))
  $ body $ do
  let chtransform :: (IvoryStore a1)
                  => Ix 8
                  -> (Uint16 -> Ivory eff a1)
                  -> Label "userinput_result" (Stored a1)
                  -> Ivory eff ()
      chtransform ix f ofield = deref (pwms ! (ix :: Ix 8)) >>=
                                f >>= \v -> store (ui ~> ofield) v
  chtransform 0 scale_rpy I.roll
  chtransform 1 scale_rpy I.pitch
  chtransform 2 scale_thr I.throttle
  chtransform 3 scale_rpy I.yaw
  store (ui ~> I.time) now

  armed <- arming_statemachine pwms state now
  mode  <- mode_statemachine pwms state now
  store (fm ~> FM.armed) armed
  store (fm ~> FM.mode)  mode
  store (fm ~> FM.time)  now
  retVoid

arming_statemachine :: (Ref s1 (Array 8 (Stored Uint16)))
                    -> (Ref s2 (Struct "userinput_decode_state"))
                    -> Uint32
                    -> Ivory eff IBool
arming_statemachine pwms state now = do
  ch5_switch <- deref (pwms ! (5 :: Ix 8))
  throttle_stick <- deref (pwms ! (2 :: Ix 8))
  rudder_stick   <- deref (pwms ! (3 :: Ix 8))

  ifte_ (ch5_switch <? 1500)
    (do_disarm)
    (ifte_ ((throttle_stick <? 1050) .&& (rudder_stick >? 1900))
      (do_try_arming)
      (do_not_arming))
  newstate <- (state ~>* arm_state)
  return $ (newstate ==? as_ARMED)
  where
  set_arm_state :: Uint8 -> Ivory eff ()
  set_arm_state newstate = do
    store (state ~> arm_state) newstate
    store (state ~> arm_state_time) now

  do_disarm :: Ivory eff ()
  do_disarm = set_arm_state as_DISARMED

  hystresis = 500

  do_try_arming :: Ivory eff ()
  do_try_arming = do
    ast <- deref (state ~> arm_state)
    astime <- deref (state ~> arm_state_time)
    (ifte_ (ast ==? as_DISARMED)
      (when (now - astime >? hystresis)
        (set_arm_state as_ARMING))
      (when (ast ==? as_ARMING)
        (when (now - astime >? hystresis)
          (set_arm_state as_ARMED))))

  do_not_arming :: Ivory eff ()
  do_not_arming = do
    ast <- deref (state ~> arm_state)
    when (ast ==? as_ARMING)
      (set_arm_state as_DISARMED)

mode_statemachine :: (Ref s1 (Array 8 (Stored Uint16)))
                  -> (Ref s2 (Struct "userinput_decode_state"))
                  -> Uint32
                  -> Ivory eff Uint8
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
  mode_from_pwm :: Uint16 -> Uint8
  mode_from_pwm pwm = foldr matchmodemap FM.flightModeStabilize mode_pwm_map
    -- Build up a series of conditional checks using fold. Default to
    -- mode_STABILIZE if none found.
    where
    matchmodemap (mode, (minpwm, maxpwm)) dflt =
      ((pwm >=? minpwm) .&& (pwm <=? maxpwm)) ? (mode, dflt)

  magnitude :: Uint16 -> Uint16 -> Uint16
  magnitude a b = castDefault
                $ abs $ (safeCast a :: Sint32) - (safeCast b)

scale_thr :: Uint16 -> Ivory eff IFloat
scale_thr input = call scale_proc 1000 1000 0.0 1.0 input

scale_rpy :: Uint16 -> Ivory eff IFloat
scale_rpy input = call scale_proc 1500 500 (-1.0) 1.0 input

scale_proc :: Def ('[Uint16, Uint16, IFloat, IFloat, Uint16] :-> IFloat)
scale_proc = proc "userinput_scale" $ \center range outmin outmax input ->
  requires (range /=? 0) $ body $ do
    let centered = input - center
    let ranged = (safeCast centered) / (safeCast range)
    ifte_ (ranged <? outmin)
      (ret outmin)
      (ifte_ (ranged >? outmax)
        (ret outmax)
        (ret ranged))


userInputFailsafe :: Def ('[ Ref s1 (Struct "userinput_result")
                           , Ref s2 (Struct "flightmode")
                           , Uint32 ] :-> ())
userInputFailsafe = proc "userinput_failsafe" $ \capt fm now ->
  requires (checkStored (capt ~> I.time) (\t -> now >=? t))
  $ body $ do
    last <- deref ( capt ~> I.time )
    let dt = now - last
    when (dt >? 150) $ do
       store (fm   ~> FM.armed)   false
       store (capt ~> I.throttle) 0
       store (capt ~> I.yaw)      0
       store (capt ~> I.pitch)    0
       store (capt ~> I.roll)     0
    retVoid

