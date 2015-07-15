{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.IO
  ( flightIOTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C ()

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.IO.PPM
import SMACCMPilot.Hardware.PPM (ppmTower)
import SMACCMPilot.Hardware.PX4IO (px4ioTower)

flightIOTower :: (e -> FlightPlatform)
              -> ChanInput (Struct "user_input")
              -> ChanInput (Struct "control_law")
              -> ChanOutput (Struct "control_law")
              -> ChanOutput (Struct "quadcopter_motors")
              -> Tower e ()
flightIOTower tofp ppm_ui ppm_cl cl_output motors_output = do
  fp <- fmap tofp getEnv
  ppm_raw <- channel
  case fp_io fp of
    PX4IO dmauart pins -> do
      px4io_state <- channel
      px4ioTower tocc dmauart pins cl_output motors_output (fst px4io_state)
      -- XXX FIXME: need a decoder to hook up px4io_state to ppm_raw!

    NativeIO ppm_hw ->
      ppmTower (const ppm_hw) tocc (fst ppm_raw)

  ppmInputTower (snd ppm_raw) ppm_ui ppm_cl

  where
  tocc = fp_clockconfig . tofp
