{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Control.PID where

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.PidState as P
import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig as C

controlPIDModule :: Module
controlPIDModule = package "control_pid" $ do
  depend C.pidConfigTypesModule
  depend P.pidStateTypesModule
  incl fconstrain
  incl pid_update
  incl pid_reset

----------------------------------------------------------------------

notFloatNan :: IFloat -> IBool
notFloatNan flt = (iNot $ isnan flt) .&& (iNot $ isinf flt)

-- | Update a PID controller given an error value and measured value
-- and return the output value.
-- See https://wiki.paparazziuav.org/wiki/Control_Loops#Attitude_loop for details
pid_update :: Def ('[ Ref      s1 ('Struct "pid_state")
                    , ConstRef s2 ('Struct "pid_config")
                    , IFloat -- float angle_ref [rad]
                    , IFloat -- float angle_measured [rad]
                    , IFloat -- float rate_ref [rad/s]
                    , IFloat -- float rate_measured [rad/s]
                    , IFloat -- float accel_ref [rad/s^2]
                    , IFloat -- dt [s]
                    ]':-> IFloat) -- float stabilization_cmd
pid_update = proc "pid_update" $ \pid cfg angle_ref angle_measured rate_ref rate_measured accel_ref dt ->
  requires (notFloatNan angle_ref) $ requires (notFloatNan angle_measured) $ requires (notFloatNan rate_ref) $ requires (notFloatNan rate_measured) $ requires (notFloatNan accel_ref)
  $ body $ do
  -- load gains and limits
  p_gain <- cfg~>*C.p_gain
  i_gain  <- cfg~>*C.i_gain
  i_min   <- cfg~>*C.i_min
  i_max   <- cfg~>*C.i_max
  d_gain  <- cfg~>*C.d_gain
  dd_gain  <- cfg~>*C.dd_gain
  err_max  <- cfg~>*C.err_max
  errd_max  <- cfg~>*C.errd_max

  -- calculate errors
  angle_err <- ifte (err_max >? 0.0) (call fconstrain (-err_max) err_max (angle_ref - angle_measured)) (return (angle_ref - angle_measured))
  store (pid~>P.angle_err) angle_err
  rate_err <- ifte (errd_max >? 0.0) (call fconstrain (-errd_max) errd_max (rate_ref - rate_measured)) (return (rate_ref - rate_measured))
  store (pid~>P.rate_err) rate_err

  -- calculate terms
  let p_term = p_gain * angle_err
  store (pid~>P.p_term) p_term

  let d_term = d_gain * rate_err
  store (pid~>P.d_term) d_term

  let dd_term = dd_gain * accel_ref
  store (pid~>P.dd_term) dd_term

  i_sum <- pid~>*P.i_state
  i_sum' <- call fconstrain i_min i_max (i_sum + angle_err*dt)
  store (pid~>P.i_state) i_sum'

  let i_term = i_gain * i_sum'
  store (pid~>P.i_term) i_term

  ret $ p_term + i_term + d_term + dd_term

-- | Reset the internal state of a PID.
pid_reset :: Def ('[ Ref s1 ('Struct "pid_state") ] ':-> ())
pid_reset = proc "pid_reset" $ \pid -> body $ do
  store (pid ~> P.i_state) 0.0
  store (pid ~> P.p_term) 0.0
  store (pid ~> P.i_term) 0.0
  store (pid ~> P.d_term) 0.0
  store (pid ~> P.dd_term) 0.0

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] ':-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

