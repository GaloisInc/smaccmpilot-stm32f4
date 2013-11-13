{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SMACCMPilot.Flight.Control.AltHold where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Flight.Types.UserInput as UI
import qualified SMACCMPilot.Flight.Types.Sensors   as S

----------------------------------------------------------------------
-- Utilities

[ivory|
struct maybe_float
  { mf_valid              :: Stored IBool
  ; mf_value              :: Stored IFloat
  }
|]

instance MaybeType "maybe_float" IFloat where
  maybeValidLabel = mf_valid
  maybeValueLabel = mf_value

----------------------------------------------------------------------
-- Constants

alt_hold_dt :: Floating a => a
alt_hold_dt = 1.0 / 200.0

alt_hold_throttle_dt :: Floating a => a
alt_hold_throttle_dt = 1.0 / 200.0

-- | User input stick throttle dead band.
throttle_dead_band :: IFloat
throttle_dead_band = 0.25

alt_hold_accel_max :: IFloat
alt_hold_accel_max = 2.5

alt_hold_velocity_max :: IFloat
alt_hold_velocity_max = 2.5

-- XXX what should these really be?
throttle_min, throttle_max :: IFloat
throttle_min = 0.01
throttle_max = 0.99

----------------------------------------------------------------------
-- Altitude Hold Controller

-- | Altitude hold controller state.
[ivory|
struct alt_hold_state
  { ah_throttle_cruise    :: Stored IFloat
  ; ah_throttle_avg       :: Struct "maybe_float"
  ; ah_target_alt         :: Struct "maybe_float"
  ; ah_rate_filter        :: Struct "maybe_float"
  ; ah_accel_filter       :: Struct "maybe_float"
  ; ah_speed_filter       :: Struct "maybe_float"
  ; ah_throttle_pid       :: Struct "PIDState"

  ; ah_desired_rate       :: Stored IFloat
  ; ah_alt_error          :: Stored IFloat
  ; ah_target_rate        :: Stored IFloat
  ; ah_current_rate       :: Stored IFloat
  ; ah_error_rate         :: Stored IFloat
  ; ah_target_accel       :: Stored IFloat
  ; ah_angle_boost        :: Stored IFloat
  }
|]

-- | Return the change in a value, filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassDeriv :: Float                         -- freq
             -> Float                         -- dt
             -> IFloat                        -- new value
             -> Ref s (Struct "maybe_float")  -- prev value
             -> Ivory eff IFloat              -- filtered delta
lowPassDeriv freq dt x prev = do
  let rc     = 1.0 / (2.0 * pi * freq)
  let alpha  = ifloat (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return dx

-- | Return a changing value filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassFilter :: Float                        -- freq
              -> Float                        -- dt
              -> IFloat                       -- new value
              -> Ref s (Struct "maybe_float") -- prev value
              -> Ivory eff IFloat             -- filtered value
lowPassFilter freq dt x prev = do
  _ <- lowPassDeriv freq dt x prev
  getMaybe (constRef prev) x

-- | Get the desired climb rate (m/s) given a stick input.
--
-- XXX major bogus alert, need to multiply by another parameter to get
-- m/s.  right now we're just returning a scaled [-1.0, 1.0] value.
desiredClimbRate :: Def ('[IFloat] :-> IFloat)
desiredClimbRate = proc "desired_climb_rate" $ \stick -> body $ do
  offset     <- assign (signum stick * throttle_dead_band)
  scale      <- assign (1.0 / (1.0 - throttle_dead_band))
  ret $ (abs stick <? throttle_dead_band)
         ? (0.0, (stick - offset) * scale)

-- | Calculate alt hold climb rate given altitude error.
altHoldRate :: Def ('[ ConstRef s1 (Struct "PIDConfig")
                     , IFloat
                     ] :-> IFloat)
altHoldRate = proc "alt_hold_rate" $ \alt_pid err -> body $ do
  rate   <- local izero
  alt_kP <- deref (alt_pid ~> pid_pGain)
  ldist  <- assign (alt_hold_accel_max / (2.0 * alt_kP * alt_kP))
  let a_max = alt_hold_accel_max
  let v_max = alt_hold_velocity_max

  cond_
    [ err >? 2.0 * ldist ==>
      store rate (sqrt (2.0 * a_max * (err - ldist)))
    , err <? (-2.0) * ldist ==> do
      store rate (negate (sqrt (2.0 * a_max * (negate err - ldist))))
    , true ==> do
      store rate (err * alt_kP)
    ]
  low  <- assign (negate v_max - 2.5)
  high <- assign (v_max + 2.5)
  ret =<< call fconstrain low high =<< deref rate

-- | Update cruise throttle if we're in a level hover.
updateThrottleCruise :: Def ('[ Ref      s1 (Struct "alt_hold_state")
                              , ConstRef s2 (Struct "sensors_result")
                              , IFloat    -- throttle
                              , IFloat    -- climb_rate
                              ] :-> ())
updateThrottleCruise = proc "update_throttle_cruise" $
  \state sensors throttle climb_rate -> body $ do
  cruise <- deref (state ~> ah_throttle_cruise)
  setDefault_ (state ~> ah_throttle_avg) cruise

  -- XXX lots of magic constants here
  -- XXX check if we are level too
  when (throttle >? 0.0 .&& abs climb_rate <? 0.6) $ do
    forMaybeM (state ~> ah_throttle_avg) $ \x -> do
      x' <- assign $ x * 0.99 + throttle * 0.01
      store (state ~> ah_throttle_cruise) x'
      return x'

-- | Update target altitude given desired climb rate and dt.
updateTargetAlt :: Def ('[ Ref      s1 (Struct "alt_hold_state")
                         , ConstRef s2 (Struct "sensors_result")
                         , IFloat     -- throttle
                         , IFloat     -- climb_rate
                         ] :-> IFloat)
updateTargetAlt = proc "update_target_alt" $
  \state sensors throttle climb_rate -> body $ do
  current_alt <- deref (sensors ~> S.baro_alt)

  -- XXX only update altitude if throttle hasn't hit its limit.
  when (climb_rate /=? 0.0) $ do
    target_alt  <- getMaybe (constRef $ state ~> ah_target_alt) current_alt
    -- XXX magic numbers
    low         <- assign (current_alt - 7.5)
    high        <- assign (current_alt + 7.5)
    target_alt' <- call fconstrain low high (target_alt + (climb_rate * alt_hold_dt))
    setJust (state ~> ah_target_alt) target_alt'

  ret =<< getMaybe (constRef $ state ~> ah_target_alt) current_alt

-- XXX hardcoded
accelFilter = lowPassFilter 2.0 alt_hold_throttle_dt
rateFilter  = lowPassFilter 2.0 alt_hold_dt
speedFilter = lowPassDeriv  2.0 alt_hold_dt

-- | Update throttle output given target acceleration.
altHoldThrottle :: Def ('[ Ref      s1 (Struct "alt_hold_state")
                         , ConstRef s2 (Struct "sensors_result")
                         , ConstRef s3 (Struct "PIDConfig")
                         , IFloat     -- target_accel
                         ] :-> IFloat)
altHoldThrottle = proc "alt_hold_throttle" $
  \state sensors pid target_accel -> body $ do
  raw_accel  <- deref (sensors ~> S.zacc)
  -- Calculate earth frame Z-acceleration.
  meas_accel <- assign ((raw_accel * 9.81 / 1000.0) + 9.81)
  let err     = target_accel - meas_accel

  err_filt   <- accelFilter err (state ~> ah_accel_filter)
  -- XXX arducopter does p, i, d separately and jimmies the I value
  -- in certain conditions.
  output     <- call pid_update (state ~> ah_throttle_pid) pid err meas_accel
  cruise     <- deref (state ~> ah_throttle_cruise)
  ret =<< call fconstrain 0.0 1.0 (output + cruise)

-- | Main altitude hold controller.
altHoldController :: Def ('[ Ref      s1 (Struct "alt_hold_state")
                           , ConstRef s2 (Struct "userinput_result")
                           , ConstRef s3 (Struct "sensors_result")
                           , ConstRef s4 (Struct "PIDConfig")
                           , ConstRef s5 (Struct "PIDConfig")
                           , IFloat     -- throttle
                           , IFloat     -- climb_rate
                        ] :-> IFloat)
altHoldController = proc "alt_hold_controller" $
  \state input sensors alt_pid rate_pid throttle climb_rate -> body $ do
  stick         <- deref (input ~> UI.throttle)
  stick_rate    <- call desiredClimbRate stick

  target_alt    <- call updateTargetAlt state sensors throttle stick_rate
  current_alt   <- deref (sensors ~> S.baro_alt)
  error_alt     <- assign (target_alt - current_alt)

  target_rate   <- call altHoldRate alt_pid error_alt
  current_rate  <- assign climb_rate
  error_rate    <- rateFilter (target_rate - current_rate) (state ~> ah_rate_filter)

  store (state ~> ah_desired_rate) stick_rate
  store (state ~> ah_alt_error)    error_alt
  store (state ~> ah_target_rate)  target_rate
  store (state ~> ah_current_rate) current_rate
  store (state ~> ah_error_rate)   error_rate

  -- Feed forward acceleration based on change in filtered
  -- desired speed.
  delta_speed   <- speedFilter target_rate (state ~> ah_speed_filter)
  accel         <- assign (delta_speed * alt_hold_dt)
  rate_kP       <- deref (rate_pid ~> pid_pGain)
  p             <- assign (error_rate * rate_kP)
  target_accel  <- call fconstrain (-320.0) 320.0 (accel + p)
  store (state ~> ah_target_accel) target_accel

  ret target_accel

-- | Initialize controller state when entering this flight mode.
altHoldInit :: Def ('[ Ref s1 (Struct "alt_hold_state") ] :-> ())
altHoldInit = proc "alt_hold_init" $ \state -> body $ do
  store (state ~> ah_throttle_cruise) 0.44    --- XXX use param
  setNothing (state ~> ah_throttle_avg)
  setNothing (state ~> ah_target_alt)
  setNothing (state ~> ah_rate_filter)
  setNothing (state ~> ah_accel_filter)
  setNothing (state ~> ah_speed_filter)
  call_ pid_reset (state ~> ah_throttle_pid)

----------------------------------------------------------------------
-- Ivory Module

altHoldModule :: Module
altHoldModule = package "alt_hold" $ do
  depend controlPIDModule
  depend S.sensorsTypeModule
  depend UI.userInputTypeModule
  -- XXX some of these should be private
  defStruct (Proxy :: Proxy "maybe_float")
  defStruct (Proxy :: Proxy "alt_hold_state")
  incl desiredClimbRate
  incl updateThrottleCruise
  incl updateTargetAlt
  incl altHoldRate
  incl altHoldController
  incl altHoldThrottle
  incl altHoldInit
