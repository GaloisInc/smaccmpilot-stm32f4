

module SMACCMPilot.Flight.UserInput
  ( userInputTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.UserInput.PPM

import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw      as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult   as S
import qualified SMACCMPilot.Comm.Ivory.Types.TimeMicros      as T
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput       as UI ()
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputResult as UIR

import SMACCMPilot.Comm.Tower.Attr
import SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

userInputTower :: (e -> FlightPlatform)
               -> ControllableVehicleAttrs Attr
               -> Tower e ()
userInputTower tofp attrs = do

  (ppm_ui, ppm_cl) <- ppmInputTower toppm tocc

  monitor "user_input_multiplexer" $ do
    ppm_cl_s      <- state "ppm_cl"

    telem_cl      <- attrState (controlLawRequest attrs)

    telem_ui      <- state "telem_ui"
    telem_ui_time <- state "telem_ui_time"

    imu_valid     <- state "imu_valid"
    attrHandler (userInputRequest attrs) $ do
      callback $ \v -> do
        refCopy telem_ui v
        now <- getTime
        store telem_ui_time now

    -- Only set imu_valid once we have a valid sensor output solution.
    -- Don't ever set imu_valid from true to false- would cause us to fall
    -- out of the sky.
    attrHandler (sensorsOutput attrs) $ do
      callback $ \o -> do
        v <- deref (o ~> S.valid)
        when v $ store imu_valid true

    -- Invariant: ppm_cl and ppm_ui are emitted on every tick of the same period
    -- thread. ppm_cl is delivered before ppm_ui.
    handler ppm_cl "ppm_cl" $ do
      callback $ \v -> refCopy ppm_cl_s v

    handler ppm_ui "ppm_ui" $ do
      e_cl <- attrEmitter (controlLaw attrs)
      e_ui <- attrEmitter (userInput attrs)
      callback $ \ppm_ui_s -> do
        now <- getTime
        now_telem <- assign (T.TimeMicros (toIMicroseconds now))
        ui_o <- local (istruct [ UIR.time .= ival now_telem])
        cl_o <- local (istruct [])

        -- Select the control law:
        ppm_not_safe    <- fmap (/=? A.safe) (deref (ppm_cl_s ~> CL.arming_mode))
        ppm_allows_gcs  <- fmap (==? CS.gcs) (deref (ppm_cl_s ~> CL.ui_mode))
        telem_wants_gcs <- fmap (==? CS.gcs) (deref (telem_cl ~> CL.ui_mode))
        cond_
          [ ppm_not_safe .&& ppm_allows_gcs .&& telem_wants_gcs ==> do
              refCopy cl_o telem_cl
          , true ==> do
              refCopy cl_o ppm_cl_s
          ]

        -- Select the user input source, using the same rule as control law
        -- but also checking to make sure we recently got a fresh ui over
        -- telemetry.
        ui_time <- deref telem_ui_time
        recent_telem_ui <- assign ((now - ui_time) <=? timeout)
        cond_
          [ ppm_allows_gcs .&& telem_wants_gcs .&& recent_telem_ui  ==> do
              refCopy (ui_o ~> UIR.ui)     telem_ui
              store   (ui_o ~> UIR.source) CS.gcs
          , true ==> do
              refCopy (ui_o ~> UIR.ui)     ppm_ui_s
              store   (ui_o ~> UIR.source) CS.ppm
          ]

        -- Guard arming on imu_valid
        a_desired <- deref (cl_o ~> CL.arming_mode)
        i_valid <- deref imu_valid
        when (a_desired ==? A.armed .&& iNot i_valid) $ do
            store (cl_o ~> CL.arming_mode) A.disarmed

        emit e_ui (constRef ui_o)
        emit e_cl (constRef cl_o)

  where
  tocc = fp_clockconfig . tofp
  toppm = fp_ppm . tofp

  timeout :: ITime
  timeout = fromIMilliseconds (500 :: Sint16)

