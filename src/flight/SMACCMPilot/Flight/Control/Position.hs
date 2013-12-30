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
import qualified SMACCMPilot.Flight.Types.ControlLaw      as CL
import qualified SMACCMPilot.Flight.Types.ArmedMode       as A
import qualified SMACCMPilot.Flight.Types.PosControlDebug ()

import           SMACCMPilot.Flight.Control.PID

-- import           SMACCMPilot.Flight.Control.Position.Velocity

data PositionControl =
  PositionControl
    { pos_init   :: forall eff . Ivory eff ()
    , pos_update :: forall eff s1 s2 s3
                  . Ref s1 (Struct "sensors_result")
                 -> Ref s2 (Struct "position")
                 -> Ref s3 (Struct "control_law")
                 -> IFloat
                 -> Ivory eff ()
    , pos_output :: forall eff . Ivory (AllocEffects eff)
                                       (IBool, IFloat, IFloat, IFloat)
    }

taskPositionControl :: PosCtlParams ParamReader
                    -> DataSource  (Struct "pos_control_dbg")
                    -> Task p PositionControl
taskPositionControl param_reader s_pos_dbg = do
  f <- fresh
  posDbgWriter   <- withDataWriter s_pos_dbg "pos_control_dbg"

  let named n = "pos_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        -- RESET & WHATEVER
        return ()

      update_proc :: Def ('[ Ref s1 (Struct "sensors_result")
                           , Ref s2 (Struct "position")
                           , Ref s3 (Struct "control_law")
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens pos cl dt -> body $ do
        armed    <- deref (cl ~> CL.armed_mode)
        active   <- assign (armed ==? A.armed) -- XXX
        thrust_pid_cfg  <- allocPIDParams (posCtlThrust param_reader)

        when active $ do
          -- RUN STUFF
          return ()

        unless active $ do
          -- RESET STUFF
          return ()

        call_ debug_proc

      debug_proc :: Def ('[]:->())
      debug_proc = proc (named "debug") $ body $ do
        dbg <- local izero
        -- XXX
        writeData posDbgWriter (constRef dbg)

      output_proc :: Def ('[ Ref s1 (Stored IBool)
                           , Ref s2 (Stored IFloat)
                           , Ref s3 (Stored IFloat)
                           , Ref s4 (Stored IFloat)]:->())
      output_proc = proc (named "output") $ \v ix iy iz -> body $ do
        store v false
        store ix 0
        store iy 0
        store iz 0

  taskModuleDef $ do
    incl init_proc
    incl update_proc
    incl debug_proc
    incl output_proc
  return PositionControl
    { pos_init   = call_ init_proc
    , pos_update = call_ update_proc
    , pos_output = do
        v <- local (ival false)
        x <- local (ival 0)
        y <- local (ival 0)
        z <- local (ival 0)
        call_ output_proc v x y z
        vv <- deref v
        xx <- deref x
        yy <- deref y
        zz <- deref z
        return (vv,xx,yy,zz)
    }


