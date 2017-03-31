{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law.ControlModes where

import           Ivory.Language
import           Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS

controlModesTower :: ChanOutput ('Struct "control_modes") -- CLOCK
                  -> ChanOutput ('Struct "control_modes")
                  -> ChanInput  ('Struct "control_modes")
                  -> Tower e ()
controlModesTower rcinput_modes telem_modes output_modes = monitor "control_modes_law" $ do
  telem_request <- state "telem_control_modes_request"
  handler telem_modes "telem_control_modes" $ do
    callback $ \m -> do
      refCopy telem_request m

  handler rcinput_modes "rcinput_control_modes" $ do
    e <- emitter output_modes 1
    callback $ \m -> do
      rcin_allows_gcs <- fmap (==? CS.gcs) (deref (m ~> CM.ui_mode))
      telem_wants_gcs <- fmap (==? CS.gcs) (deref (telem_request ~> CM.ui_mode))

      ifte_ (rcin_allows_gcs .&& telem_wants_gcs)
            (emit e (constRef telem_request))
            (do m' <- local izero
                refCopy m' m
                store (m' ~> CM.ui_mode) CS.ppm
                emit e (constRef m'))

controlModesTowerRCOnly :: ChanOutput ('Struct "control_modes")
                        -> ChanOutput ('Struct "control_modes")
                        -> ChanInput  ('Struct "control_modes")
                        -> Tower e ()
controlModesTowerRCOnly rcinput_modes _telem_modes output_modes =
  monitor "control_modes_rc_only_law" $ do
    handler rcinput_modes "rcinput_control_modes" $ do
      e <- emitter output_modes 1
      callback $ \m -> do
        m' <- local izero
        refCopy m' m
        emit e (constRef m')
