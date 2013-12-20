{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}


module SMACCMPilot.Flight.Core
  ( core
  , FlightCoreRequires(..)
  , FlightCoreProvides(..)
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib (stdlibModules)

import qualified Ivory.BSP.STM32F4.GPIO as GPIO
import           Ivory.HXStream

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import           SMACCMPilot.Mavlink.Send     (mavlinkSendModule)
import           SMACCMPilot.Mavlink.Receive  (mavlinkReceiveStateModule)
import           SMACCMPilot.Mavlink.CRC      (mavlinkCRCModule)
import           SMACCMPilot.Mavlink.Pack     (packModule)
import           SMACCMPilot.Mavlink.Messages.VehCommsec (vehCommsecModule)

import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver (senderModules)
import           SMACCMPilot.Flight.BlinkTask
import           SMACCMPilot.Flight.Control
import           SMACCMPilot.Flight.Motors.Task
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Flight.Types (typeModules)
import           SMACCMPilot.Flight.UserInput

import           SMACCMPilot.Param

data FlightCoreRequires =
  FlightCoreRequires
    { sensors_in        :: DataSink (Struct "sensors_result")
    , params_in         :: FlightParams ParamSink
    , rcoverride_in     :: ChannelSink 16 (Struct "rc_channels_override_msg")
    , ctl_req_in        :: ChannelSink 16 (Struct "control_law_request")
    }

data FlightCoreProvides =
  FlightCoreProvides
    { control_out      :: ChannelSink 16 (Struct "controloutput")
    , motors_out       :: ChannelSink 16 (Struct "motors")
    , controllaw_state :: DataSink (Struct "control_law")
    , alt_ctl_state    :: DataSink (Struct "alt_control_dbg")
    , att_ctl_state    :: DataSink (Struct "att_control_dbg")
    , userinput_state  :: DataSink (Struct "userinput_result")
    }

core :: FlightCoreRequires -> Tower p FlightCoreProvides
core sys = do
  motors  <- channel
  control <- channel
  alt_ctl <- dataport
  att_ctl <- dataport

  (userinput_chan, controllaw_chan) <- userInputTower
                                          (ctl_req_in sys)
                                          (rcoverride_in sys)
  userinput  <- stateProxy "proxy_userinput" userinput_chan
  controllaw <- stateProxy "proxy_controllaw" controllaw_chan

  task "blink"      $ blinkTask lights controllaw
  task "control"    $ controlTask
                        controllaw
                        userinput
                        (sensors_in sys)
                        (src control)
                        (src alt_ctl)
                        (src att_ctl)
                        (params_in sys)
  task "motmix"     $ motorMixerTask
                        (snk control)
                        controllaw
                        (src motors)

  addDepends vehCommsecModule
  mapM_ addDepends typeModules
  mapM_ addModule otherms

  return $ FlightCoreProvides
    { control_out      = snk control
    , motors_out       = snk motors
    , controllaw_state = controllaw
    , alt_ctl_state    = snk alt_ctl
    , att_ctl_state    = snk att_ctl
    , userinput_state  = userinput
    }
  where
  lights = [relaypin, redledpin]
  relaypin = GPIO.pinB13
  redledpin = GPIO.pinB14

  otherms :: [Module]
  otherms = concat
    -- flight types
    [ typeModules
    -- control subsystem
    , controlModules
    -- mavlink system
    , mavlinkMessageModules
    -- standard library
    , stdlibModules
    ] ++
    [ packModule
    , mavlinkCRCModule
    , paramModule
    -- the rest:
    -- hxstream
    , hxstreamModule
    -- Used in channels
    , senderModules
    , mavlinkSendModule
    , mavlinkReceiveStateModule
    ]

