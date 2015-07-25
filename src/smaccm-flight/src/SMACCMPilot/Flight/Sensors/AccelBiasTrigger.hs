{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Sensors.AccelBiasTrigger
  ( accelBiasTriggerTower
  ) where

import Ivory.Language
import Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioState  as PX4
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioStatus as PX4

accelBiasTriggerTower :: ChanOutput (Stored IBool)
                      -> ChanOutput (Struct "px4io_state")
                      -> ChanInput  (Stored IBool)
                      -> Tower e ()
accelBiasTriggerTower motion_chan px4_chan trigger_chan = monitor "accel_bias_trigger" $ do

  trigger_event <- state "trigger_event"

  handler motion_chan "accel_motion" $ do
    e <- emitter trigger_chan 1
    callbackV $ \mot -> do
      t_evt <- deref trigger_event
      store trigger_event false
      ifte_ (t_evt .&& iNot mot)
            (emitV e true)
            (emitV e false)

  last_safety_state <- state "last_safety_state"
  handler px4_chan "px4_state" $ do
    callback $ \s -> do
      safety_off <- deref (s ~> PX4.status ~> PX4.safety_off)
      safety_was_off <- deref last_safety_state
      ifte_ (safety_off .&& iNot safety_was_off)
            (store trigger_event true)
            (store trigger_event false)

      store last_safety_state safety_off
