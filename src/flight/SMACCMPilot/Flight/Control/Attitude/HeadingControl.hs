{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Attitude.HeadingControl
  ( HeadingController(..)
  , taskHeadingControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.Param

import           SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Flight.Types.Sensors         as S
import qualified SMACCMPilot.Flight.Types.AttControlDebug as ACD

data HeadingController =
  HeadingController
    { hctl_init   :: forall eff . Ivory eff ()
    , hctl_update :: forall eff s
                   . IFloat
                  -> IFloat
                  -> Ref s (Struct "sensors_result")
                  -> IFloat
                  -> Ivory eff ()
    , hctl_reset :: forall eff . Ivory eff ()
    , hctl_setpoint :: forall eff . Ivory eff IFloat
    , hctl_write_debug :: forall eff s . Ref s (Struct "att_control_dbg")
                     -> Ivory eff ()
    }

taskHeadingControl :: PIDParams ParamReader -> Task p HeadingController
taskHeadingControl params = do
  uniq <- fresh
  let named n = "head_ctl_" ++ n ++ "_" ++ show uniq
  pid_state  <- taskLocal "headingPIDState"
  output     <- taskLocal "headingOutput"
  let proc_update :: Def('[ IFloat -- Heading setpoint
                          , IFloat -- Rate setpoint
                          , Ref s (Struct "sensors_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc (named "update") $
        \head_setpt rate_setpt sens dt -> body $ do
          pid_params <- allocPIDParams params
          p_gain <-             (pid_params~>*pid_pGain)
          d_gain <- fmap (/ dt) (pid_params~>*pid_dGain)

          head_est <- deref (sens ~> S.yaw)
          -- XXX omega_z assumes body frame and world frame aligned
          rate_est <- deref (sens ~> S.omega_z)
          pos_err  <- assign (angledomain (head_setpt - head_est))
          vel_err  <- assign (rate_setpt - rate_est)

          pid_result <- assign ((p_gain * pos_err) - (d_gain * vel_err))
          store output (pid_result + rate_setpt)

      proc_reset :: Def('[]:->())
      proc_reset = proc (named "reset") $ body $ do
        call_ pid_reset pid_state

  taskModuleDef $ do
    incl proc_update
    incl proc_reset
    depend controlPIDModule
  return HeadingController
    { hctl_init     = call_ proc_reset
    , hctl_update   = call_ proc_update
    , hctl_reset    = call_ proc_reset
    , hctl_setpoint = deref output
    , hctl_write_debug = \acd -> do
        p <- deref (pid_state ~> pid_pLast)
        d <- deref (pid_state ~> pid_dLast)
        store (acd ~> ACD.head_ctl_p) p
        store (acd ~> ACD.head_ctl_d) d
    }





-- Take an angle in radians which is outside of linear range (-pi, pi] by less
-- than pi, and maps it to an equal angle in that range.
angledomain :: IFloat -> IFloat
angledomain a =
  ( a <=? (-pi) ) ? ( a + 2*pi
  , (a >? pi ) ? ( a - 2*pi
    , a))



