{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4IOAR where

import Ivory.Language
import Ivory.Tower

px4ioarTower :: (SingI n)
             => ChannelSink n (Array 4 (Stored IFloat))
             -> Tower p ()
px4ioarTower = const (return ())

 -- change motor mixer [0.0f..1.0f] range to to [0..500]
scale_period :: IFloat -> Uint16
scale_period f = castWith 0 (500*f)

ardrone_motor_set :: ConstRef s (Array 4 (Stored Uint16)) -> Ivory eff ()
ardrone_motor_set periods = return () -- XXX

ardrone_pins_init :: Ivory eff ()
ardrone_pins_init = return () -- XXX

-- motor_init: state machine that requires timed waiting
