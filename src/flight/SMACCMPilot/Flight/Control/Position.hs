{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Position
  ( PositionControl(..)
  , taskPositionControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Hardware.GPS.Types           ()
import qualified SMACCMPilot.Flight.Types.Sensors         ()
import qualified SMACCMPilot.Flight.Types.PosControlDebug ()

import           SMACCMPilot.Flight.Control.StatePID

data PositionControl =
  PositionControl
    { pos_init   :: forall eff . Ivory eff ()
    , pos_update :: forall eff s1 s2
                  . Ref s1 (Struct "sensors_result")
                 -> Ref s2 (Struct "position")
                 -> IFloat
                 -> Ivory eff ()
    , pos_reset  :: forall eff . Ivory eff ()
    , pos_output :: forall eff cs . (GetAlloc eff ~ Scope cs)
                 => Ivory eff (IBool, IFloat, IFloat)
    }

taskPositionControl :: PosCtlParams ParamReader
                    -> DataSource  (Struct "pos_control_dbg")
                    -> Task p PositionControl
taskPositionControl param_reader s_pos_dbg = do
  f <- fresh
  posDbgWriter <- withDataWriter s_pos_dbg "pos_control_dbg"

  x_vel_pid <- taskStatePID (posCtlThrust param_reader) "x_velocity"
  y_vel_pid <- taskStatePID (posCtlThrust param_reader) "y_velocity"
  active <- taskLocal "active"

  let named n = "pos_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        spid_init x_vel_pid
        spid_init y_vel_pid

      update_proc :: Def ('[ Ref s1 (Struct "sensors_result")
                           , Ref s2 (Struct "position")
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens pos dt -> body $ do
        a <- deref active
        unless a $ do
          store active true

        x_vel_est <- assign 0 -- XXX
        y_vel_est <- assign 0 -- XXX
        spid_update x_vel_pid 0 x_vel_est dt
        spid_update y_vel_pid 0 y_vel_est dt

        call_ debug_proc

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        spid_reset x_vel_pid
        spid_reset y_vel_pid

      debug_proc :: Def ('[]:->())
      debug_proc = proc (named "debug") $ body $ do
        dbg <- local izero
        -- XXX
        writeData posDbgWriter (constRef dbg)

      output_proc :: Def ('[ Ref s1 (Stored IBool)
                           , Ref s2 (Stored IFloat)
                           , Ref s3 (Stored IFloat)
                           ]:->())
      output_proc = proc (named "output") $ \v rx ry -> body $ do
        a <- deref active
        store v a
        when a $ do
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
  return PositionControl
    { pos_init   = call_ init_proc
    , pos_update = call_ update_proc
    , pos_reset  = call_ reset_proc
    , pos_output = do
        v <- local (ival false)
        x <- local (ival 0)
        y <- local (ival 0)
        call_ output_proc v x y
        vv <- deref v
        xx <- deref x
        yy <- deref y
        return (vv,xx,yy)
    }


