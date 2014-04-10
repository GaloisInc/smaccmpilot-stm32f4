{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Navigation.Velocity
  ( VelocityControl(..)
  , taskVelocityControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Hardware.GPS.Types           as P
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import qualified SMACCMPilot.Flight.Types.PosControlDebug as D

import           SMACCMPilot.Flight.Control.StateDerivativePID

data VelocityControl =
  VelocityControl
    { vel_init   :: forall eff . Ivory eff ()
    , vel_update :: forall eff s1 s2
                  . ConstRef s1 (Struct "sensors_result")
                 -> ConstRef s2 (Struct "position")
                 -> IFloat -- Setpoint X
                 -> IFloat -- Setpoint Y
                 -> IFloat -- dt
                 -> Ivory eff ()
    , vel_reset  :: forall eff . Ivory eff ()
    , vel_output :: forall eff cs . (GetAlloc eff ~ Scope cs)
                 => Ivory eff (IFloat, IFloat)
    , vel_debug  :: forall eff s . Ref s (Struct "pos_control_dbg")
                 -> Ivory eff ()
    }

taskVelocityControl :: PosCtlParams ParamReader
                    -> Task p VelocityControl
taskVelocityControl param_reader = do
  f <- fresh
  -- NED aerospace frame:
  -- x, ahead, points north at 0 heading
  -- y, right side, points east at 0 heading
  x_vel_pid <- taskStateDerivativePID (posCtlThrust param_reader) "x_velocity"
  y_vel_pid <- taskStateDerivativePID (posCtlThrust param_reader) "y_velocity"
  g_x_vel_est <- taskLocal "x_vel_est"
  g_y_vel_est <- taskLocal "y_vel_est"

  -- World frame (x=north, y=east) position cache, for calculating acceleration
  (last_pos_vx   :: Ref Global (Stored IFloat)) <- taskLocal "last_pos_vx"
  (last_pos_vy   :: Ref Global (Stored IFloat)) <- taskLocal "last_pos_vy"
  (last_pos_ax   :: Ref Global (Stored IFloat)) <- taskLocal "last_pos_ax"
  (last_pos_ay   :: Ref Global (Stored IFloat)) <- taskLocal "last_pos_ay"
  (last_pos_time :: Ref Global (Stored ITime)) <- taskLocal "last_pos_time"

  let named n = "vel_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        sdpid_init x_vel_pid
        sdpid_init y_vel_pid

      update_proc :: Def ('[ ConstRef s1 (Struct "sensors_result")
                           , ConstRef s2 (Struct "position")
                           , IFloat
                           , IFloat
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens pos set_x set_y dt -> body $ do
        (x_acc_est, y_acc_est) <- axy sens pos
        (x_vel_est, y_vel_est) <- vxy sens pos
        sdpid_update x_vel_pid set_x x_vel_est x_acc_est dt
        sdpid_update y_vel_pid set_y y_vel_est y_acc_est dt
        store g_x_vel_est x_vel_est
        store g_y_vel_est y_vel_est

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        sdpid_reset x_vel_pid
        sdpid_reset y_vel_pid

      debug_proc :: Def ('[Ref s (Struct "pos_control_dbg")]:->())
      debug_proc = proc (named "debug") $ \dbg -> body $ do
        (xp, xi, xd) <- sdpid_debug x_vel_pid
        (yp, yi, yd) <- sdpid_debug y_vel_pid
        x_vel_est <- deref g_x_vel_est
        y_vel_est <- deref g_y_vel_est

        store (dbg ~> D.x_vel_est) x_vel_est
        store (dbg ~> D.x_vel_p) xp
        store (dbg ~> D.x_vel_i) xi
        store (dbg ~> D.x_vel_d) xd
        store (dbg ~> D.y_vel_est) y_vel_est
        store (dbg ~> D.y_vel_p) yp
        store (dbg ~> D.y_vel_i) yi
        store (dbg ~> D.y_vel_d) yd

      output_proc :: Def ('[ Ref s1 (Stored IFloat)
                           , Ref s2 (Stored IFloat)
                           ]:->())
      output_proc = proc (named "output") $ \rx ry -> body $ do
        x <- sdpid_output x_vel_pid
        y <- sdpid_output y_vel_pid
        store rx x
        store ry y

      axy :: (GetAlloc eff ~ Scope s)
          => ConstRef s1 (Struct "sensors_result")
          -> ConstRef s2 (Struct "position")
          -> Ivory eff (IFloat, IFloat)
      axy sens pos = do
        t_prev <- deref last_pos_time
        t_this <- deref (pos ~> P.time)
        (int_dt :: Sint32) <- assign (castWith 0 (toIMilliseconds
                                      (t_this - t_prev)))
        dt     <- assign ((safeCast int_dt) / 1000.0)
        when (t_this >? t_prev) $ do
          (this_vx, this_vy) <- world_vxy pos
          prev_vx <- deref last_pos_vx
          prev_vy <- deref last_pos_vy
          ax <- assign ((this_vx - prev_vx) / dt)
          ay <- assign ((this_vy - prev_vy) / dt)
          store last_pos_time t_this
          store last_pos_vx   this_vx
          store last_pos_vy   this_vy
          store last_pos_ax   ax
          store last_pos_ay   ay
        ax <- deref last_pos_ax
        ay <- deref last_pos_ay
        rotate_by sens ax ay

  taskModuleDef $ do
    incl init_proc
    incl update_proc
    incl reset_proc
    incl debug_proc
    incl output_proc
  return VelocityControl
    { vel_init   = call_ init_proc
    , vel_update = call_ update_proc
    , vel_reset  = call_ reset_proc
    , vel_output = do
        x <- local (ival 0)
        y <- local (ival 0)
        call_ output_proc x y
        xx <- deref x
        yy <- deref y
        return (xx,yy)
    , vel_debug  = call_ debug_proc
    }

-- NED aerospace frame:
-- x, ahead, points north at 0 heading
-- y, right side, points east at 0 heading

vxy :: ConstRef s1 (Struct "sensors_result")
    -> ConstRef s2 (Struct "position")
    -> Ivory eff (IFloat, IFloat)
vxy sens pos = do
  (v_north, v_east) <- world_vxy pos
  rotate_by sens v_north v_east

world_vxy :: ConstRef s (Struct "position") 
          -> Ivory eff (IFloat, IFloat)
world_vxy pos = do
  vn_cms  <- deref (pos ~> P.vnorth)
  ve_cms  <- deref (pos ~> P.veast)
  v_north <- assign ((safeCast vn_cms) / 100.0)
  v_east  <- assign ((safeCast ve_cms) / 100.0)
  return (v_north, v_east)

rotate_by :: ConstRef s (Struct "sensors_result")
          -> IFloat -> IFloat
          -> Ivory eff (IFloat, IFloat)
rotate_by sens v_north v_east = do
  heading <- deref (sens ~> S.yaw)
  c_head  <- assign (cos heading)
  s_head  <- assign (sin heading)
  vx <- assign (     c_head * v_north + s_head * v_east)
  vy <- assign (-1 * s_head * v_north + c_head * v_east)
  return (vx, vy)
