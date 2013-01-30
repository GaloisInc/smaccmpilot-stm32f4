{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module UserInputDecode where

import Ivory.Language

import IvoryHelpers

import qualified UserInputType as I

userInputDecodeModule :: Module
userInputDecodeModule = package "userinput_decode" $ do
  depend I.userInputModule
  defStruct (Proxy :: Proxy "userinput_decode_state")
  incl decode
  incl scale_proc

[ivory|
struct userinput_decode_state
  { last_modepwm      :: Stored Uint16
  ; last_modepwm_time :: Stored Uint32
  ; arm_state         :: Stored Uint8
  ; arm_state_time    :: Stored Uint32
  }
|]

as_DISARMED, as_ARMING, as_ARMED :: Uint8
as_DISARMED = 0
as_ARMING   = 1
as_ARMED    = 2

mode_STABILIZE, mode_ALT_HOLD, mode_LOITER :: Uint8
mode_STABILIZE = 0
mode_ALT_HOLD  = 1
mode_LOITER    = 2

mode_pwm_map :: [(Uint8, (Uint16, Uint16))]    -- on ER9X this should be:
mode_pwm_map = [(mode_LOITER,    (900, 1300))  -- AUX 3 up
               ,(mode_ALT_HOLD,  (1301, 1700)) -- AUX 3 center
               ,(mode_STABILIZE, (1701, 2100)) -- AUX 3 down
               ]

decode :: Def ('[ Ref s1 (Array 8 (Stored Uint16))
                , Ref s2 (Struct "userinput_decode_state")
                , Ref s3 (Struct "userinput_result")
                , Uint32 ] :-> ())
decode = proc "userinput_decode" $ \pwms state out now -> body $ do
  let chtransform ix f ofield = deref (pwms ! (ix :: Ix Uint8 8)) >>=
                                f >>= \v -> store (out ~> ofield) v
  store (out ~> I.time) now
  chtransform 0 scale_rpy I.roll
  chtransform 1 scale_rpy I.pitch
  chtransform 2 scale_thr I.throttle
  chtransform 3 scale_rpy I.yaw
  arming_statemachine pwms state out now
  mode_statemachine pwms state out now
  retVoid

arming_statemachine :: (Ref s1 (Array 8 (Stored Uint16)))
                    -> (Ref s2 (Struct "userinput_decode_state"))
                    -> (Ref s3 (Struct "userinput_result"))
                    -> Uint32
                    -> Ivory s () ()
arming_statemachine pwms state out now = do
  ch5_switch <- deref (pwms ! (5 :: Ix Uint8 8))
  throttle_stick <- deref (pwms ! (2 :: Ix Uint8 8))
  rudder_stick   <- deref (pwms ! (3 :: Ix Uint8 8))

  ifte (ch5_switch <? 1500)
    (do_disarm)
    (ifte ((throttle_stick <? 1050) .&& (rudder_stick >? 1900))
      (do_try_arming)
      (do_not_arming))
  where
  set_arm_state :: Uint8 -> Ivory s () ()
  set_arm_state newstate = do
    store (state ~> arm_state) newstate
    store (state ~> arm_state_time) now
    ifte (newstate ==? as_DISARMED)
      (store (out ~> I.armed) false)
      (ift (newstate ==? as_ARMED)
        (store (out ~> I.armed) true))

  do_disarm :: Ivory s () ()
  do_disarm = set_arm_state as_DISARMED
  
  hystresis = 500

  do_try_arming :: Ivory s () ()
  do_try_arming = do
    as <- deref (state ~> arm_state)
    astime <- deref (state ~> arm_state_time)
    (ifte (as ==? as_DISARMED)
      (ift (now - astime >? hystresis)
        (set_arm_state as_ARMING))
      (ift (as ==? as_ARMING)
        (ift (now - astime >? hystresis)
          (set_arm_state as_ARMED))))

  do_not_arming :: Ivory s () ()
  do_not_arming = do
    as <- deref (state ~> arm_state)
    ift (as ==? as_ARMING)
      (set_arm_state as_DISARMED)

mode_statemachine :: (Ref s1 (Array 8 (Stored Uint16)))
                  -> (Ref s2 (Struct "userinput_decode_state"))
                  -> (Ref s3 (Struct "userinput_result"))
                  -> Uint32
                  -> Ivory s () ()
mode_statemachine pwms state out now = do
  mode_input_current <- deref (pwms ! (4 :: Ix Uint8 8))
  mode_input_prev    <- deref (state ~> last_modepwm)
  prev_time          <- deref (state ~> last_modepwm_time)
  let pwmtolerance = 10
  let latchtime    = 250
  ifte ((abs (mode_input_current - mode_input_prev)) <? pwmtolerance)
    (ift (now - prev_time >? latchtime)
      (newmode mode_input_current))
    (reset_input mode_input_current)
  where
  reset_input m = do
    store (state ~> last_modepwm) m
    store (state ~> last_modepwm_time) now
  newmode m = do 
    reset_input m
    new <- assign $ mode_from_pwm m
    store (out ~> I.mode) new
  mode_from_pwm :: Uint16 -> Uint8
  mode_from_pwm pwm = foldr matchmodemap mode_STABILIZE mode_pwm_map
    -- Build up a series of conditional checks using fold. Default to
    -- mode_STABILIZE if none found.
    where
    matchmodemap (mode, (minpwm, maxpwm)) dflt =
      ((pwm >=? minpwm) .&& (pwm <=? maxpwm)) ? (mode, dflt)

scale_thr :: Uint16 -> Ivory s a IFloat
scale_thr input = call scale_proc 1000 1000 0.0 1.0 input

scale_rpy :: Uint16 -> Ivory s a IFloat
scale_rpy input = call scale_proc 1500 500 (-1.0) 1.0 input

scale_proc :: Def ('[Uint16, Uint16, IFloat, IFloat, Uint16] :-> IFloat)
scale_proc = proc "userinput_scale" $ \center range outmin outmax input -> body $ do
  let centered = input - center
  let ranged = (toFloat centered) / (toFloat range)
  ifte (ranged <? outmin)
    (ret outmin)
    (ifte (ranged >? outmax)
      (ret outmax)
      (ret ranged))
  

