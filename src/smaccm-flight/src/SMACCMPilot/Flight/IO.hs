{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.IO
  ( flightIOTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioState as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput as I ()
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as C ()
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

import SMACCMPilot.Flight.Platform
import SMACCMPilot.Flight.IO.RCInput
import SMACCMPilot.Hardware.PPM (ppmTower)
import SMACCMPilot.Hardware.PX4IO (px4ioTower)

flightIOTower :: (e -> FlightPlatform)
              -> ControllableVehicleAttrs Attr
              -> ChanInput (Struct "user_input")
              -> ChanInput (Struct "control_law")
              -> ChanOutput (Struct "control_law")
              -> ChanOutput (Struct "quadcopter_motors")
              -> Tower e ()
flightIOTower tofp attrs rc_ui rc_cl cl_output motors_output = do
  fp <- fmap tofp getEnv
  rc_input <- channel
  case fp_io fp of
    PX4IO dmauart pins -> do
      px4io_state <- channel

      px4ioTower tocc dmauart pins cl_output motors_output (fst px4io_state)

      monitor "px4io_rcinput_translator" $ do
        handler (snd px4io_state) "new_px4iostate" $ do
          e <- emitter (fst rc_input) 1
          e_s <- attrEmitter (px4ioState attrs)
          callback $ \s -> do
            emit e (s ~> PX4.rc_in)
            emit e_s s

    NativeIO ppm_hw ->
      ppmTower (const ppm_hw) tocc (fst rc_input)

  rcInputTower attrs (snd rc_input) rc_ui rc_cl

  where
  tocc = fp_clockconfig . tofp
