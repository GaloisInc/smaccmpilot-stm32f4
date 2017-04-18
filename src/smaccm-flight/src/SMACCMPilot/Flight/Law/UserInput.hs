{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Law.UserInput where

import           Ivory.Language
import           Ivory.Tower
import           SMACCMPilot.Time
import qualified SMACCMPilot.Comm.Ivory.Types.ControlModes    as CM
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource   as CS
import qualified SMACCMPilot.Comm.Ivory.Types.UserInput       as UI
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
            (do blended_ui <- local izero
                -- blend the control inputs except for throttle, which
                -- stays entirely with the RC controller for safety
                rc_thr      <- deref (rcinput_ui ~> UI.throttle)
                telem_roll  <- deref (telem_ui   ~> UI.roll)
                rc_roll     <- deref (rcinput_ui ~> UI.roll)
                telem_pitch <- deref (telem_ui   ~> UI.pitch)
                rc_pitch    <- deref (rcinput_ui ~> UI.pitch)
                telem_yaw   <- deref (telem_ui   ~> UI.yaw)
                rc_yaw      <- deref (rcinput_ui ~> UI.yaw)
                let blended_thr   = rc_thr
                    blended_roll  = fconstrain (-1.0) 1.0 (telem_roll + rc_roll)
                    blended_pitch = fconstrain (-1.0) 1.0 (telem_pitch + rc_pitch)
                    blended_yaw   = fconstrain (-1.0) 1.0 (telem_yaw + rc_yaw)
                store (blended_ui ~> UI.throttle) blended_thr
                store (blended_ui ~> UI.roll)     blended_roll
                store (blended_ui ~> UI.pitch)    blended_pitch
                store (blended_ui ~> UI.yaw)      blended_yaw
                refCopy (uir ~> UIR.ui)     blended_ui
                store   (uir ~> UIR.source) CS.gcs)
            (do refCopy (uir ~> UIR.ui)     rcinput_ui
                store   (uir ~> UIR.source) CS.ppm)
      emit e (constRef uir)
  where
  timeout :: ITime
  timeout = fromIMilliseconds (400 :: Sint16)

-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: IFloat -> IFloat -> IFloat -> IFloat
fconstrain xmin xmax x = (x <? xmin) ? (xmin, (x >? xmax) ? (xmax, x))
