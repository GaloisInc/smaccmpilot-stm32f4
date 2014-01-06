{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Navigation.Velocity
  ( VelocityControl(..)
  , taskVelocityControl
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Hardware.GPS.Types           as P
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import qualified SMACCMPilot.Flight.Types.PosControlDebug as D

import           SMACCMPilot.Flight.Control.StatePID

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
  x_vel_pid <- taskStatePID (posCtlThrust param_reader) "x_velocity"
  y_vel_pid <- taskStatePID (posCtlThrust param_reader) "y_velocity"
  g_x_vel_est <- taskLocal "x_vel_est"
  g_y_vel_est <- taskLocal "y_vel_est"

  let named n = "vel_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        spid_init x_vel_pid
        spid_init y_vel_pid

      update_proc :: Def ('[ ConstRef s1 (Struct "sensors_result")
                           , ConstRef s2 (Struct "position")
                           , IFloat
                           , IFloat
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens pos set_x set_y dt -> body $ do
        (x_vel_est, y_vel_est) <- vxy sens pos
        spid_update x_vel_pid set_x x_vel_est dt
        spid_update y_vel_pid set_y y_vel_est dt
        store g_x_vel_est x_vel_est
        store g_y_vel_est y_vel_est

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        spid_reset x_vel_pid
        spid_reset y_vel_pid

      debug_proc :: Def ('[Ref s (Struct "pos_control_dbg")]:->())
      debug_proc = proc (named "debug") $ \dbg -> body $ do
        (xp, xi, xd) <- spid_debug x_vel_pid
        (yp, yi, yd) <- spid_debug y_vel_pid
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
        x <- spid_output x_vel_pid
        y <- spid_output y_vel_pid
        store rx x
        store ry y


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
  vn_cms  <- deref (pos ~> P.vnorth)
  ve_cms  <- deref (pos ~> P.veast)
  v_north <- assign ((safeCast vn_cms) / 100.0)
  v_east  <- assign ((safeCast ve_cms) / 100.0)
  heading <- deref (sens ~> S.yaw)

  c_head  <- assign (cos heading)
  s_head  <- assign (sin heading)
  vx <- assign (     c_head * v_north + s_head * v_east)
  vy <- assign (-1 * s_head * v_north + c_head * v_east)
  return (vx, vy)
