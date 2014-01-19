{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Navigation.Failsafe
  ( Failsafe(..)
  , taskFailsafe
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Param
import SMACCMPilot.Flight.Param

import qualified SMACCMPilot.Flight.Types.CommsecStatus   as C
import qualified SMACCMPilot.Flight.Types.NavLaw          as NL
import qualified SMACCMPilot.Flight.Types.Sensors         as S

data Failsafe =
  Failsafe
    { fs_init   :: forall eff . Ivory eff ()
    , fs_update :: forall eff s1 s2
                 . Ref s1 (Struct "nav_law")
                -> Ref s2 (Struct "sensors_result")
                -> Ivory eff ()
    , fs_active :: forall eff . Ivory eff IBool
    }

taskFailsafe :: FlightParams ParamSink
             -> DataSink (Stored C.CommsecStatus)
             -> Task p Failsafe
taskFailsafe params commsec_snk = do
  f <- fresh
  active <- taskLocal "nav_fs_active"
  target <- taskLocal "nav_fs_target"
  commsec_stat_reader <- withDataReader commsec_snk "commsec_status"
  fs_alt_param <- paramReader (flightFSAlt params)
  let named n = "nav_fs_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        store active false

      update_proc :: Def ('[ Ref s1 (Struct "nav_law")
                           , Ref s2 (Struct "sensors_result")
                           ]:->())
      update_proc = proc (named "update") $ \ nlaw sens -> body $ do
        commsec_stat_ref <- local izero
        readData commsec_stat_reader commsec_stat_ref
        commsec_stat <- deref commsec_stat_ref

        when (commsec_stat ==? C.alarm) $ do
          activated <- deref active
          unless activated $ do
            cur_alt <- deref (sens ~> S.baro_alt)
            fs_offs <- paramGet fs_alt_param
            store target (cur_alt + fs_offs)

          store active true
          store (nlaw ~> NL.velocity_control) false
          store (nlaw ~> NL.position_control) false
          store (nlaw ~> NL.heading_control) false
          store (nlaw ~> NL.altitude_control) true
          store (nlaw ~> NL.alt_setpt) =<< deref target
          store (nlaw ~> NL.alt_rate_setpt) 1.5

  taskModuleDef $ do
    incl init_proc
    incl update_proc
  return Failsafe
    { fs_init = call_ init_proc
    , fs_update = call_ update_proc
    , fs_active = deref active
    }

