{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ESCCal.ESCCalApp (app) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Flight.IO
import SMACCMPilot.Flight.Datalink.ControllableVehicle
import SMACCMPilot.Flight.Platform
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode as A
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as CL
import qualified SMACCMPilot.Comm.Ivory.Types.QuadcopterMotors as M
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as UI

app :: (e -> FlightPlatform)
    -> Tower e ()
app tofp = do
  (attrs, _streams) <- controllableVehicleAPI

  rcin_ui <- channel
  rcin_cm <- channel
  rcin_am <- channel

  motors <- channel
  control_law <- channel

  monitor "arm_now_please" $ do
    handler systemInit "init" $ do
      e <- emitter (fst control_law) 1
      callback $ \_ -> do
        v <- local $ istruct [ CL.arming_mode .= ival A.armed
                             , CL.control_modes .= izero ]
        emit e (constRef v)

  flightIOTower tofp attrs
                (fst rcin_ui)
                (fst rcin_cm)
                (fst rcin_am)
                (snd control_law)
                (snd motors)

  monitor "throttle_passthrough" $ do
    handler (snd rcin_ui) "rcin_ui" $ do
      e <- emitter (fst motors) 1
      callback $ \ui -> do
        thr <- deref (ui ~> UI.throttle)
        let thr' = (thr + 1) / 2
        v <- local $ istruct [ M.frontleft .= ival thr'
                             , M.frontright .= ival thr'
                             , M.backleft .= ival thr'
                             , M.backright .= ival thr'
                             ]
        emit e (constRef v)
