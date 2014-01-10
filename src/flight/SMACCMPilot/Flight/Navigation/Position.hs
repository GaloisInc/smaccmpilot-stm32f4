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
-- import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

-- import qualified SMACCMPilot.Hardware.GPS.Types           as P
import qualified SMACCMPilot.Flight.Types.Sensors()
import qualified SMACCMPilot.Flight.Types.PosControlDebug as D
import qualified SMACCMPilot.Flight.Types.UserInput       as UI


data PositionControl =
  PositionControl
    { pos_init   :: forall eff . Ivory eff ()
    , pos_update :: forall eff s1 s2 s3
                  . ConstRef s1 (Struct "sensors_result")
                 -> ConstRef s2 (Struct "position")
                 -> ConstRef s3 (Struct "userinput_result")
                 -> IFloat
                 -> Ivory eff ()
    , pos_reset  :: forall eff . Ivory eff ()
    , pos_output :: forall eff cs . (GetAlloc eff ~ Scope cs)
                 => Ivory eff (IFloat, IFloat)
    , pos_debug  :: forall eff s . Ref s (Struct "pos_control_dbg")
                 -> Ivory eff ()
    }

taskPositionControl :: PosCtlParams ParamReader
                    -> Task p PositionControl
taskPositionControl _param_reader = do
  f <- fresh

  pit_in <- taskLocal "pitch_input"
  rll_in <- taskLocal "roll_input"

  let named n = "pos_ctl_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        return ()

      update_proc :: Def ('[ ConstRef s1 (Struct "sensors_result")
                           , ConstRef s2 (Struct "position")
                           , ConstRef s3 (Struct "userinput_result")
                           , IFloat
                           ]:->())
      update_proc = proc (named "update") $ \_sens _pos ui _dt -> body $ do
        -- XXX STUB FOR TESTING VELOCITY CTL
        store pit_in =<< deref (ui ~> UI.pitch)
        store rll_in =<< deref (ui ~> UI.roll)

      reset_proc :: Def ('[]:->())
      reset_proc = proc (named "reset") $ body $ do
        -- XXX UNIMPLEMENTED
        return ()

      debug_proc :: Def ('[Ref s (Struct "pos_control_dbg")]:->())
      debug_proc = proc (named "debug") $ \dbg -> body $ do
        store (dbg ~> D.x_vel_setpt) =<< deref pit_in
        store (dbg ~> D.y_vel_setpt) =<< deref rll_in
        store (dbg ~> D.head_setpt) 9999 -- invalid
        store (dbg ~> D.lat_setpt) 9999 -- invalid
        store (dbg ~> D.lon_setpt) 9999 -- invalid
        store (dbg ~> D.x_deviation) 9999 -- invalid
        store (dbg ~> D.y_deviation) 9999 -- invalid

      output_proc :: Def ('[ Ref s1 (Stored IFloat)
                           , Ref s2 (Stored IFloat)
                           ]:->())
      output_proc = proc (named "output") $ \rx ry -> body $ do
        -- XXX STUB FOR TESTING VELOCITY CTL
        store rx =<< deref pit_in
        store ry =<< deref rll_in

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
    , pos_debug = call_ debug_proc
    }
