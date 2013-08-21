{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PX4IOAR where

import Ivory.Language
import Ivory.Tower

px4ioarTower :: (SingI n)
             => ChannelSink n (Array 4 (Stored Uint16))
             -> Tower p ()
px4ioarTower = const (return ())

