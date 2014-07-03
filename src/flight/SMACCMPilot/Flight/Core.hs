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
import Ivory.Serialize (serializeModule)

import qualified Ivory.BSP.STM32F405.GPIO as GPIO
import           Ivory.HXStream

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import           SMACCMPilot.Mavlink.Send     (mavlinkSendModule)
import           SMACCMPilot.Mavlink.Receive  (mavlinkReceiveStateModule)
import           SMACCMPilot.Mavlink.CRC      (mavlinkCRCModule)
import           SMACCMPilot.Mavlink.Messages.VehCommsec (vehCommsecModule)

import           SMACCMPilot.Flight.GCS.Transmit.MessageDriver (senderModules)
import           SMACCMPilot.Flight.BlinkTask
import           SMACCMPilot.Flight.Control
import           SMACCMPilot.Flight.Navigation
import           SMACCMPilot.Flight.Motors.Task
import           SMACCMPilot.Flight.Param
import           SMACCMPilot.Flight.Types (typeModules)
import           SMACCMPilot.Flight.Types.CommsecStatus (CommsecStatus)
import           SMACCMPilot.Flight.UserInput

import           SMACCMPilot.Param

data FlightCoreRequires =
  FlightCoreRequires
    { sensors_in        :: ChannelSink (Struct "sensors_result")
    , position_in       :: ChannelSink (Struct "position")
    , params_in         :: FlightParams ParamSink
    , rcoverride_in     :: ChannelSink (Struct "rc_channels_override_msg")
    , navcommand_in     :: ChannelSink (Struct "nav_command")
    , ctl_req_in        :: ChannelSink (Struct "control_law_request")
    , commsec_mon_in    :: ChannelSink (Stored CommsecStatus)
    }

data FlightCoreProvides =
  FlightCoreProvides
    { control_out      :: ChannelSink (Struct "controloutput")
    , motors_out       :: ChannelSink (Struct "motors")
    , controllaw_state :: ChannelSink (Struct "control_law")
    , pos_ctl_state    :: ChannelSink (Struct "pos_control_dbg")
    , alt_ctl_state    :: ChannelSink (Struct "alt_control_dbg")
    , att_ctl_state    :: ChannelSink (Struct "att_control_dbg")
    , userinput_state  :: ChannelSink (Struct "userinput_result")
    , navlaw_state     :: ChannelSink (Struct "nav_law")
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
    , nav_commsec_mon = commsec_mon_in sys
    }

  ctl <- controlTower (params_in sys) ControlInputs
    { ci_law   = controllaw_chan
    , ci_ui    = userinput_chan
    , ci_setpt = nav_setpt nav
    , ci_sens  = sensors_in sys
    }

  task "blink"  $ blinkTask lights controllaw_chan (commsec_mon_in sys)
  task "motmix" $ motorMixerTask
                        (co_ctl ctl)
                        controllaw_chan
                        (src motors)

  towerDepends vehCommsecModule
  mapM_ towerDepends typeModules
  mapM_ towerModule otherms

  return $ FlightCoreProvides
    { control_out      = co_ctl ctl
    , motors_out       = snk motors
    , controllaw_state = controllaw_chan
    , pos_ctl_state    = nav_pos_dbg nav
    , alt_ctl_state    = co_alt_dbg ctl
    , att_ctl_state    = co_att_dbg ctl
    , userinput_state  = userinput_chan
    , navlaw_state     = nav_law nav
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
    ++ [ serializeModule
       , mavlinkCRCModule
       , paramModule
       -- hxstream
       , hxstreamModule
       -- Used in channels
       , senderModules
       , mavlinkSendModule
       , mavlinkReceiveStateModule
       ]

