{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.Flight.Control
  ( controlTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ControlOutput   as CO
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSetpoint as SP
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput       as UI
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputResult as UIR
import qualified SMACCMPilot.Comm.Ivory.Types.YawMode         as Y
import qualified SMACCMPilot.Comm.Ivory.Types.AttControlDebug as ACD
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult as SEN
import qualified SMACCMPilot.Comm.Ivory.Types.Xyz           as XYZ
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle


import           SMACCMPilot.Flight.Control.Attitude.Stabilize (attStabilizeModule)
import           SMACCMPilot.Flight.Control.PID
import           SMACCMPilot.Flight.Control.Altitude
import           SMACCMPilot.Flight.Control.Attitude.PitchRoll
import           SMACCMPilot.Flight.Control.Yaw
import           SMACCMPilot.Flight.Control.Attitude.YawUI
import           SMACCMPilot.Flight.Types.MaybeFloat

controlTower :: ControllableVehicleAttrs Attr
             -> Tower e ()
controlTower attrs = do
  p <- period dt
  monitor "control" $ do
    cl     <- attrState (controlLaw attrs)
    ui_res <- attrState (userInput attrs)
    sens   <- attrState (sensorsOutput attrs)
    setpt  <- attrState (controlSetpoint attrs)
    att_dbg <- attrState (attControlDebug attrs)

    alt_control    <- monitorAltitudeControl  attrs
    prc_control    <- monitorPitchRollControl attrs
    yaw_control    <- monitorYawControl       attrs
    yui            <- monitorYawUI

    handler systemInit "control_init" $ callback $ const $ do
      alt_init alt_control
      prc_init prc_control
      yaw_init yaw_control

    handler p "control_periodic" $ do
      e <- attrEmitter (controlOutput attrs)
      alt_debug_e <- attrEmitter (altControlDebug attrs)
      att_debug_e <- attrEmitter (attControlDebug attrs)
      callback $ const $ do
        let ui = (ui_res ~> UIR.ui)
        -- Run altitude and attitude controllers
        alt_update alt_control sens ui setpt cl idt

        armed   <- deref (cl ~> CL.arming_mode)
        ui_mode <- deref (cl ~> CL.control_modes ~> CM.ui_mode)

        cond_
          [ armed /=? A.armed ==>
              prc_reset prc_control
          , ui_mode /=? CS.nav ==> do
              pit_ui <- deref (ui ~> UI.pitch)
              rll_ui <- deref (ui ~> UI.roll)
              let pitchroll_ui_sens_dps = 45.0
              ui_sens_rads <- assign (pitchroll_ui_sens_dps * pi / 180)
              prc_run prc_control (pit_ui * ui_sens_rads)
                                  (rll_ui * ui_sens_rads)
                                  (constRef sens)
              store (att_dbg ~> ACD.pitch_setpt) (-1 * pit_ui * ui_sens_rads)
              store (att_dbg ~> ACD.roll_setpt) (rll_ui * ui_sens_rads)
              prc_debug prc_control att_dbg
          , ui_mode ==? CS.nav ==> do
              pit_sp <- deref (setpt ~> SP.pitch)
              rll_sp <- deref (setpt ~> SP.roll)
              prc_run prc_control pit_sp rll_sp (constRef sens)
          ]

        yaw_mode <- deref (cl ~> CL.control_modes ~> CM.yaw_mode)
        cond_
          [ armed /=? A.armed ==> do
              yui_reset yui
              yaw_reset yaw_control
          , yaw_mode ==? Y.rate ==> do
              yui_reset yui
              rate_sp <- deref (ui ~> UI.yaw)
              let yaw_ui_sens_dps = 180.0
              ui_sens_rads <- assign (yaw_ui_sens_dps * pi / 180)
              let yaw_rate_sp = (ui_sens_rads * rate_sp)
              yaw_rate yaw_control sens yaw_rate_sp idt
              store (att_dbg ~> ACD.head_rate_setpt) yaw_rate_sp
              sen_omega_z <- deref ((sens ~> SEN.omega) ~> XYZ.z)
              store (att_dbg ~> ACD.head_rate_est) sen_omega_z
          , yaw_mode ==? Y.heading .&& ui_mode /=? CS.nav ==> do
              yui_update yui sens ui idt
              (head_sp, head_rate_sp) <- yui_setpoint yui
              yaw_heading yaw_control sens head_sp head_rate_sp idt
          , yaw_mode ==? Y.heading .&& ui_mode ==? CS.nav ==> do
              yui_reset yui
              head_sp <- deref (setpt ~> SP.heading)
              yaw_heading yaw_control sens head_sp 0 idt
          ]


        -- Defaults for disarmed:
        ctl <- local $ istruct
          [ CO.throttle .= ival 0
          , CO.roll     .= ival 0
          , CO.pitch    .= ival 0
          , CO.yaw      .= ival 0
          , CO.armed    .= ival false
          ]

        when (armed ==? A.armed) $ do
          alt_output alt_control ctl
          prc_state  prc_control ctl
          yaw_output yaw_control ctl
          store (ctl ~> CO.armed) true

        emit e (constRef ctl)

        alt_debug_v <- local izero
        alt_debug alt_control alt_debug_v
        emit alt_debug_e (constRef alt_debug_v)
        emit att_debug_e (constRef att_dbg)

  mapM_ towerModule controlModules

  where
  dt = Milliseconds 5
  idt :: IFloat
  idt = (safeCast s16_ms) / 1000.0
    where
    -- We need to cast to a smaller integer type to be able to safely
    -- convert to a float. toIMilliseconds gives sint64 but really we are
    -- using very few of those bits of precision
    s16_ms :: Sint16
    s16_ms = castWith 0 (toIMilliseconds (toITime dt))

controlModules :: [Module]
controlModules = [ controlPIDModule, attStabilizeModule, maybeFloatModule ]

