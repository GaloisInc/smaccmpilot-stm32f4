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
import           SMACCMPilot.Flight.Navigation
import           SMACCMPilot.Flight.Motors.Task
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Flight.Types (typeModules)
import           SMACCMPilot.Flight.UserInput

import           SMACCMPilot.Param

data FlightCoreRequires =
  FlightCoreRequires
    { sensors_in        :: DataSink (Struct "sensors_result")
    , position_in       :: ChannelSink 16 (Struct "position")
    , params_in         :: FlightParams ParamSink
    , rcoverride_in     :: ChannelSink 16 (Struct "rc_channels_override_msg")
    , navcommand_in     :: ChannelSink 16 (Struct "nav_command")
    , ctl_req_in        :: ChannelSink 16 (Struct "control_law_request")
    }

data FlightCoreProvides =
  FlightCoreProvides
    { control_out      :: ChannelSink 16 (Struct "controloutput")
    , motors_out       :: ChannelSink 16 (Struct "motors")
    , controllaw_state :: DataSink (Struct "control_law")
    , pos_ctl_state    :: DataSink (Struct "pos_control_dbg")
    , alt_ctl_state    :: DataSink (Struct "alt_control_dbg")
    , att_ctl_state    :: DataSink (Struct "att_control_dbg")
    , userinput_state  :: DataSink (Struct "userinput_result")
    , navlaw_state     :: DataSink (Struct "nav_law")
    }

core :: FlightCoreRequires -> Tower p FlightCoreProvides
core sys = do
  motors  <- channel
  nav_law_req_chan <- channel

  (userinput_chan, controllaw_chan) <- userInputTower UITowerInputs
    { uit_mavlink_req   = ctl_req_in sys
    , uit_mavlink_rcovr = rcoverride_in sys
    , uit_nav_req       = snk nav_law_req_chan
    }

  nav <- navTower (params_in sys) NavInputs
    { nav_law_req  = src nav_law_req_chan
    , nav_ui       = userinput_chan
    , nav_ctl_law  = controllaw_chan
    , nav_position = position_in sys
    , nav_sens     = sensors_in sys
    , nav_cmd      = navcommand_in sys
    }

  userinput  <- stateProxy "proxy_userinput" userinput_chan
  controllaw <- stateProxy "proxy_controllaw" controllaw_chan
  nav_setpt_state <- stateProxy "proxy_navsetpt" (nav_setpt nav)
  nav_law_state <- stateProxy "proxy_nav_law" (nav_law nav)

  ctl <- controlTower (params_in sys) ControlInputs
    { ci_law   = controllaw
    , ci_ui    = userinput
    , ci_setpt = nav_setpt_state
    , ci_sens  = sensors_in sys
    }

  task "blink"  $ blinkTask lights controllaw
  task "motmix" $ motorMixerTask
                        (co_ctl ctl)
                        controllaw
                        (src motors)

  addDepends vehCommsecModule
  mapM_ addDepends typeModules
  mapM_ addModule otherms

  return $ FlightCoreProvides
    { control_out      = co_ctl ctl
    , motors_out       = snk motors
    , controllaw_state = controllaw
    , pos_ctl_state    = nav_pos_dbg nav
    , alt_ctl_state    = co_alt_dbg ctl
    , att_ctl_state    = co_att_dbg ctl
    , userinput_state  = userinput
    , navlaw_state     = nav_law_state
    }
  where
  lights = [relaypin, redledpin]
  relaypin = GPIO.pinB13
  redledpin = GPIO.pinB14

  otherms :: [Module]
  otherms = typeModules -- flight types
    -- mavlink system
    ++ mavlinkMessageModules
    -- standard library
    ++ stdlibModules
    ++ [ packModule
       , mavlinkCRCModule
       , paramModule
       -- hxstream
       , hxstreamModule
       -- Used in channels
       , senderModules
       , mavlinkSendModule
       , mavlinkReceiveStateModule
       ]

