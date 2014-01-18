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

data Failsafe =
  Failsafe
    { fs_init :: forall eff . Ivory eff ()
    , fs_update :: forall eff s1
                  . Ref s1 (Struct "nav_law")
                 -> Ivory eff ()
    }

taskFailsafe :: DataSink (Stored C.CommsecStatus)
             -> Task p Failsafe
taskFailsafe commsec_snk = do
  f <- fresh
  active <- taskLocal "nav_fs_active"
  commsec_stat_reader <- withDataReader commsec_snk "commsec_status"

  let named n = "nav_fs_" ++ n ++ "_" ++ show f

      init_proc :: Def ('[]:->())
      init_proc = proc (named "init") $ body $ do
        store active false

      update_proc :: Def ('[ Ref s1 (Struct "nav_law")
                           ]:->())
      update_proc = proc (named "update") $ \ nlaw -> body $ do
        commsec_stat_ref <- local izero
        readData commsec_stat_reader commsec_stat_ref
        commsec_stat <- deref commsec_stat_ref
        when (commsec_stat ==? C.alarm) $ do
          activated <- deref active
          unless activated $ do
            return ()
            -- XXX MODIFY NLAW TO INCREASE ALTITUDE TO 2M

  taskModuleDef $ do
    incl init_proc
    incl update_proc
  return Failsafe
    { fs_init = call_ init_proc
    , fs_update = call_ update_proc
    }

