{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Control.Attitude.HeadingControl
  ( HeadingController(..)
  , monitorHeadingControl
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import           SMACCMPilot.Flight.Control.PID

import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig       as C
import qualified SMACCMPilot.Comm.Ivory.Types.PidState        as P
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz             as XYZ
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
import qualified SMACCMPilot.Comm.Ivory.Types.AttControlDebug as ACD
import           SMACCMPilot.Comm.Tower.Attr

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
    , hctl_write_debug :: forall eff s . Ref s (Struct "att_control_debug")
                     -> Ivory eff ()
    }

monitorHeadingControl :: (AttrReadable a)
                      => a (Struct "pid_config")
                      -> Monitor e HeadingController
monitorHeadingControl cfg_attr = do
  let named n = fmap showUnique $ freshname $ "head_ctl_" ++ n
  pid_state  <- state "headingPIDState"
  output     <- state "headingOutput"
  cfg        <- attrState cfg_attr

  name_update <- named "update"
  name_reset <- named "reset"

  let proc_update :: Def('[ IFloat -- Heading setpoint
                          , IFloat -- Rate setpoint
                          , Ref s (Struct "sensors_result")
                          , IFloat -- dt
                          ] :-> ())
      proc_update  = proc name_update $
        \head_setpt rate_setpt sens dt -> body $ do
          p_gain <-             (cfg ~>* C.p_gain)
          d_gain <- fmap (/ dt) (cfg ~>* C.d_gain)

          head_est <- deref (sens ~> S.yaw)
          -- XXX omega_z assumes body frame and world frame aligned
          rate_est <- deref ((sens ~> S.omega) ~> XYZ.z)
          pos_err  <- assign (angledomain (head_setpt - head_est))
          vel_err  <- assign (rate_setpt - rate_est)

          pid_result <- assign ((p_gain * pos_err) - (d_gain * vel_err))
          store output (pid_result + rate_setpt)

      proc_reset :: Def('[]:->())
      proc_reset = proc name_reset $ body $ do
        call_ pid_reset pid_state

  monitorModuleDef $ do
    incl proc_update
    incl proc_reset
    depend controlPIDModule
  return HeadingController
    { hctl_init     = call_ proc_reset
    , hctl_update   = call_ proc_update
    , hctl_reset    = call_ proc_reset
    , hctl_setpoint = deref output
    , hctl_write_debug = \acd -> do
        p <- deref (pid_state ~> P.p_last)
        d <- deref (pid_state ~> P.d_last)
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



