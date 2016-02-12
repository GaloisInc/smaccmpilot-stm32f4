{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law.UserInput where

import           Ivory.Language
import           Ivory.Tower
import           SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputResult as UIR

userInputMuxTower :: ChanOutput ('Struct "control_modes")
                  -> ChanOutput ('Struct "user_input") -- CLOCK
                  -> ChanOutput ('Struct "user_input")
                  -> ChanInput  ('Struct "user_input_result")
                  -> Tower e ()
userInputMuxTower cm_chan rcinput_ui_chan telem_ui_chan output_ui_chan = monitor "user_input_mux" $ do
  telem_ui <- state "telem_ui_request"
  telem_ui_time <- state "telem_ui_request_time"
  handler telem_ui_chan "telem_ui_request" $ do
    callback $ \u -> do
      refCopy telem_ui u
      now <- getTime
      store telem_ui_time now

  cm <- state "control_modes_"
  handler cm_chan "cm" $ do
    callback $ \cm' -> refCopy cm cm'

  handler rcinput_ui_chan "rcinput_ui" $ do
    e <- emitter output_ui_chan 1
    callback $ \rcinput_ui -> do
      now             <- getTime
      telem_time      <- deref telem_ui_time
      recent_telem    <- assign ((now - telem_time) <=? timeout)

      mode_telem <- fmap (==? CS.gcs) (deref (cm ~> CM.ui_mode))

      uir <- local (istruct [ UIR.time .= ival (timeMicrosFromITime now) ])
      ifte_ (recent_telem .&& mode_telem)
            (do refCopy (uir ~> UIR.ui)     telem_ui
                store   (uir ~> UIR.source) CS.gcs)
            (do refCopy (uir ~> UIR.ui)     rcinput_ui
                store   (uir ~> UIR.source) CS.ppm)
      emit e (constRef uir)
  where
  timeout :: ITime
  timeout = fromIMilliseconds (500 :: Sint16)

