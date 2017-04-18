{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.Flight.Control.Attitude.Angle
  ( AngleControl(..)
  , monitorAngleController
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.AttControlDebug as ACD
import qualified SMACCMPilot.Comm.Ivory.Types.StabConfig as S
import           SMACCMPilot.Comm.Tower.Attr

import SMACCMPilot.Flight.Control.PID
import SMACCMPilot.Flight.Control.Attitude.Stabilize

data AngleControl =
  AngleControl
    { ac_init  :: forall eff . Ivory eff ()
    , ac_run   :: forall eff . IFloat -- Target, radians
                            -> IFloat -- Current estimate, radians
                            -> IFloat -- Derivative estimate, radians per second
                            -> Ivory eff ()
    , ac_out   :: forall eff . Ivory eff IFloat
    , ac_debug :: forall eff s . Ref s ('Struct "att_control_debug")
                              -> Ivory eff ()
    , ac_reset :: forall eff . Ivory eff ()
    }

monitorAngleController :: (AttrReadable a)
                       => a ('Struct "stab_config")
                       -> IFloat -- output range (absolute value)
                       -> String                -- name
                       -> Monitor e AngleControl
monitorAngleController stab_config_attr output_range name = do
  let named n = fmap showUnique $ freshname $ name ++ "_anglectl_" ++ n

  valid    <- state "valid"
  out      <- state "out"
  pos_pid  <- state "pos_pid"
  rate_pid <- state "rate_pid"
  last_time <- state "last_time"
  stab_config <- attrState stab_config_attr

  init_name <- named "init"
  run_name <- named "run"
  out_name <- named "out"
  debug_name <- named "debug"
  reset_name <- named "reset"

  let init_proc :: Def ('[]':->())
      init_proc = proc init_name $ body $ do
        store valid false
        call_ reset_proc

      run_proc :: Def ('[IFloat, IFloat, IFloat] ':-> ())
      run_proc = proc run_name $ \setpt est deriv_est -> body $ do
        t0 <- deref last_time
        t1 <- fmap toIMicroseconds getTime
        store last_time t1
        let dt = safeCast (castDefault (t1 - t0) :: Sint32) / 1000000
        ctl <- call stabilize_from_angle
                      pos_pid
                      (constRef (stab_config ~> S.pos))
                      setpt
                      est
                      deriv_est
                      output_range
                      dt
        store valid true
        store out ctl

      out_proc :: Def ('[] ':-> IFloat)
      out_proc = proc out_name $ body $ do
        v <- deref valid
        ifte_ v
          (deref out >>= ret)
          (ret 0)

      debug_proc :: Def ('[(Ref s ('Struct "att_control_debug"))] ':-> ())
      debug_proc = voidProc debug_name $ \dbg -> body $ do
        refCopy (dbg ~> ACD.att_pid) pos_pid

      reset_proc :: Def ('[]':->())
      reset_proc = proc reset_name $ body $ do
        store valid false
        call_ pid_reset pos_pid
        call_ pid_reset rate_pid
        t <- fmap toIMicroseconds getTime
        store last_time t

  monitorModuleDef $ do
    incl init_proc
    incl run_proc
    incl out_proc
    incl debug_proc
    incl reset_proc
    depend controlPIDModule
    depend attStabilizeModule
    -- XXX why is this not getting included by the importProc of getTime?
    dependByName "tower_time"
  return AngleControl
    { ac_init  = call_ init_proc
    , ac_run   = call_ run_proc
    , ac_out   = call  out_proc
    , ac_debug = call_ debug_proc
    , ac_reset = call_ reset_proc
    }
