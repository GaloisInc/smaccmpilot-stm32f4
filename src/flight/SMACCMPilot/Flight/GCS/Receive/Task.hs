{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  , GCSRxRequires(..)
  ) where

import           Prelude hiding (last, id)

import           Data.Traversable (traverse)

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

import qualified SMACCMPilot.Mavlink.Receive                as R

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers
import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import           SMACCMPilot.Mavlink.CRC (mavlinkCRCModule)
import qualified SMACCMPilot.Mavlink.Enums.MavCmd   as Cmd
import qualified SMACCMPilot.Communications         as Comm

--------------------------------------------------------------------------------

data GCSRxRequires =
  GCSRxRequires
    { rx_hil         :: Maybe (ChannelSource (Struct "hil_state_msg"))
    , rx_ctl_req     :: ChannelSource (Struct "control_law_request")
    , rx_param_req   :: ChannelSource (Stored Sint16)
    , rx_rc_override :: ChannelSource (Struct "rc_channels_override_msg")
    , rx_nav_command :: ChannelSource (Struct "nav_command")
    }

gcsReceiveTask :: ChannelSink   Comm.MAVLinkArray -- from decryptor
               -> ChannelSource (Struct "gcsstream_timing")
               -> [Param PortPair]
               -> GCSRxRequires
               -> Task p ()
gcsReceiveTask mavStream sper_src params req = do

  hil_emitter        <- withHILEmitter (rx_hil req)
  ctl_req_emitter    <- withChannelEmitter (rx_ctl_req     req) "ctl_req"
  param_req_emitter  <- withChannelEmitter (rx_param_req   req) "param_req"
  rcOverride_emitter <- withChannelEmitter (rx_rc_override req) "rc_override_tx"
  nav_cmd_emitter    <- withChannelEmitter (rx_nav_command req) "nav_cmd"

  -- Get lists of parameter readers and writers.
  write_params       <- traverse paramWriter (map (fmap portPairSource) params)
  read_params        <- traverse paramReader (map (fmap portPairSink)   params)

  -- Generate functions from parameter list.
  getParamIndex     <- makeGetParamIndex read_params
  setParamValue     <- makeSetParamValue write_params

  taskStackSize 4096
  streamPeriodEmitter <- withChannelEmitter sper_src "streamperiods"

  s_periods <- taskLocalInit "periods" defaultPeriods
  state     <- taskLocalInit "state"
                 (istruct [ R.status .= ival R.status_IDLE ])

  let handlerAux :: Def ('[ Ref s0 (Struct "mavlink_receive_state")
                          , ITime
                          ] :-> ())
      handlerAux = proc "gcsReceiveHandlerAux" $ \s now -> body $ do
          runHandlers s
            [ mavMsgHandle (paramRequestList read_params param_req_emitter)
            , mavMsgHandle (paramRequestRead getParamIndex param_req_emitter)
            , mavMsgHandle (paramSet getParamIndex setParamValue param_req_emitter)
            , mavMsgHandle (requestDatastream s_periods streamPeriodEmitter)
            , mavMsgHandle (rcOverride rcOverride_emitter)
            , handleCommandLong (fromIntegral Cmd.id_COMPONENT_ARM_DISARM)
                                (armDisarm ctl_req_emitter now)
            , mavMsgHandle (smaccmNavCommand nav_cmd_emitter now)
            ]
          hilhandler s
          where
          runHandlers s = mapM_ ($ s)
          hilhandler s = case hil_emitter of
            Just e -> mavMsgHandle (hilState e) s
            Nothing -> return ()

  let parseMav :: Def ('[ConstRef s1 Comm.MAVLinkArray] :-> ())
      parseMav = proc "parseMav" $ \mav -> body $ do
        arrayMap $ \ix -> do
          b <- deref (mav ! ix)
          R.mavlinkReceiveByte state b
          s <- deref (state ~> R.status)
          cond_
            [ (s ==? R.status_GOTMSG) ==> do
                t <- getTime
                call_ handlerAux state t
                R.mavlinkReceiveReset state
            , (s ==? R.status_FAIL)   ==>
                store (state ~> R.status) R.status_IDLE
            ]

  taskInit $ emit_ streamPeriodEmitter (constRef s_periods)

  mavlink_evt <- withChannelEvent mavStream "mavStream"
  handle mavlink_evt "mavlink_parse" $ \ms -> do
    mav <- local (iarray [])
    refCopy mav ms
    call_ parseMav (constRef mav)

  taskModuleDef $ do
    depend mavlinkCRCModule
    depend R.mavlinkReceiveStateModule
    depend paramModule
    handlerModuleDefs
    mapM_ depend mavlinkMessageModules
    mapM_ depend stdlibModules
    private $ do
      incl parseMav
      incl handlerAux


withHILEmitter :: Maybe (ChannelSource (Struct "hil_state_msg"))
               -> Task p (Maybe (ChannelEmitter (Struct "hil_state_msg")))
withHILEmitter (Just s) = withChannelEmitter s "hil_src" >>= \e -> return (Just e)
withHILEmitter Nothing  = return Nothing
