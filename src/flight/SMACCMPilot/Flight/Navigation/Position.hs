{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Navigation.Position
  ( PositionControl(..)
  , taskPositionControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Hardware.GPS.Types           as P
import qualified SMACCMPilot.Flight.Types.Sensors         as S
import qualified SMACCMPilot.Flight.Types.PosControlDebug as D


data PositionControl =
  PositionControl
    { pos_init   :: forall eff . Ivory eff ()
    , pos_update :: forall eff s1 s2
                  . ConstRef s1 (Struct "sensors_result")
                 -> ConstRef s2 (Struct "position")
                 -> IFloat
                 -> Ivory eff ()
    , pos_reset  :: forall eff . Ivory eff ()
    , pos_output :: forall eff cs . (GetAlloc eff ~ Scope cs)
                 => Ivory eff (IFloat, IFloat)
    }

taskPositionControl :: PosCtlParams ParamReader
                    -> DataSource  (Struct "pos_control_dbg")
                    -> Task p PositionControl
taskPositionControl param_reader s_pos_dbg = do
  f <- fresh
  posDbgWriter <- withDataWriter s_pos_dbg "pos_control_dbg"
  let named n = "pos_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        return ()

      update_proc :: Def ('[ ConstRef s1 (Struct "sensors_result")
                           , ConstRef s2 (Struct "position")
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \sens pos dt -> body $ do
        -- XXX UNIMPLEMENTED
        return ()

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        -- XXX UNIMPLEMENTED
        return ()

      debug_proc :: Def ('[]:->())
      debug_proc = proc (named "debug") $ body $ do
        dbg <- local $ istruct
          [ D.x_vel_setpt .= ival 0
          , D.y_vel_setpt .= ival 0
          , D.head_setpt  .= ival 9999 -- invalid
          , D.lat_setpt   .= ival 9999 -- invalid
          , D.lon_setpt   .= ival 9999 -- invalid
          , D.x_deviation .= ival 9999 -- invalid
          , D.y_deviation .= ival 9999 -- invalid
          ]
        writeData posDbgWriter (constRef dbg)

      output_proc :: Def ('[ Ref s1 (Stored IFloat)
                           , Ref s2 (Stored IFloat)
                           ]:->())
      output_proc = proc (named "output") $ \rx ry -> body $ do
        -- XXX UNIMPLEMENTED
        return ()

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
        x <- local (ival 0)
        y <- local (ival 0)
        call_ output_proc x y
        xx <- deref x
        yy <- deref y
        return (xx,yy)
    }
