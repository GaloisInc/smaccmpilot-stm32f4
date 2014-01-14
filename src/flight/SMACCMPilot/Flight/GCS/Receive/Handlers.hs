{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.GCS.Receive.Handlers where

import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Mavlink.Messages.RequestDataStream  as RDS
import qualified SMACCMPilot.Mavlink.Messages.ParamSet           as PS
import qualified SMACCMPilot.Mavlink.Messages.ParamRequestRead   as PRR
import qualified SMACCMPilot.Mavlink.Messages.SmaccmpilotNavCmd  as SN

import qualified SMACCMPilot.Mavlink.Receive                     as R
import qualified SMACCMPilot.Mavlink.Messages.SetMode            as SM
import qualified SMACCMPilot.Mavlink.Messages.CommandLong        as CL
import           SMACCMPilot.Mavlink.Unpack

import qualified SMACCMPilot.Mavlink.Enums.MavComponent          as MC

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.GCS.Stream (updateGCSStreamPeriods)
import qualified SMACCMPilot.Flight.Types.ControlLawRequest      as CR
import qualified SMACCMPilot.Flight.Types.NavCommand             as NC
import qualified SMACCMPilot.Flight.Types.EnableDisable          as E

--------------------------------------------------------------------------------
-- Params

-- | Send a request to the GCS transmit task to send each parameter.
paramRequestList :: (SingI n)
                 => [Param ParamReader]
                 -> ChannelEmitter n (Stored Sint16)
                 -> Ref s (Struct "param_request_list_msg")
                 -> Ivory (ProcEffects cs r) ()
paramRequestList params emitter _ =
  forM_ [0..length params] $ \n -> do
    emitV_ emitter (fromIntegral n)

-- | Request read of a parameter by name or index.
paramRequestRead :: (SingI n)
                 => ParamIndexGetter
                 -> ChannelEmitter n (Stored Sint16)
                 -> Ref s (Struct "param_request_read_msg")
                 -> Ivory (ProcEffects cs r) ()
paramRequestRead getIndex emitter msg = do
  n     <- deref (msg ~> PRR.param_index)
  ixRef <- local (ival n)

  -- Look up by name if index is -1.
  when (n ==? -1) $ do
    name <- local (stringInit "" :: Init ParamString)
    istr_from_sz name (constRef $ msg ~> PRR.param_id)
    call_ (paramIndexGetter getIndex) (constRef name) ixRef
    -- ixRef stays -1 if the name lookup fails

  ix <- deref ixRef
  unless (ix ==? -1) $ do
    emitV_ emitter ix

-- | Set a parameter's value and send the modified param info.
paramSet :: (SingI n)
         => ParamIndexGetter
         -> Def ('[Sint16, IFloat] :-> IBool)
         -> ChannelEmitter n (Stored Sint16)
         -> Ref s3 (Struct "param_set_msg")
         -> Ivory (ProcEffects cs r) ()
paramSet getIndex setValue emitter msg = do
  name  <- local (stringInit "" :: Init ParamString)
  istr_from_sz name (constRef $ msg ~> PS.param_id)

  ixRef <- local (ival (0 :: Sint16))
  found <- call (paramIndexGetter getIndex) (constRef name) ixRef

  when found $ do
    ix  <- deref ixRef
    val <- deref (msg ~> PS.param_value)
    call_  setValue ix val
    emitV_ emitter ix

--------------------------------------------------------------------------------

requestDatastream :: (SingI n, GetAlloc eff ~ Scope cs)
                  => Ref       s1 (Struct "gcsstream_timing")
                  -> ChannelEmitter n (Struct "gcsstream_timing")
                  -> Ref       s2 (Struct "request_data_stream_msg")
                  -> Ivory eff ()
requestDatastream streamperiods emitter msg = do
  rsid   <- deref (msg ~> RDS.req_stream_id)
  enable <- deref (msg ~> RDS.start_stop)
  rate   <- deref (msg ~> RDS.req_message_rate)
  updateGCSStreamPeriods streamperiods rsid (enable >? 0) rate
  emit_ emitter (constRef streamperiods)

--------------------------------------------------------------------------------

hilState :: (SingI n, GetAlloc eff ~ Scope cs)
         => ChannelEmitter n (Struct "hil_state_msg")
         -> Ref s (Struct "hil_state_msg")
         -> Ivory eff ()
hilState e r = emit_ e (constRef r)

--------------------------------------------------------------------------------

-- | Handle RC override messages.
rcOverride :: (GetAlloc eff ~ Scope cs, SingI n)
           => ChannelEmitter n (Struct "rc_channels_override_msg")
           -> Ref s (Struct "rc_channels_override_msg")
           -> Ivory eff ()
rcOverride e msgRef = emit_ e (constRef msgRef)

--------------------------------------------------------------------------------

customModeEnabled :: Uint8
customModeEnabled = 1


smaccmNavCommand :: (SingI n)
        => ChannelEmitter n (Struct "nav_command")
        -> Uint32
        -> Ref s2 (Struct "smaccmpilot_nav_cmd_msg")
        -> Ivory (ProcEffects cs r) ()
smaccmNavCommand emitter now msg = do
  autoland_active   <- deref (msg ~> SN.autoland_active)
  autoland_complete <- deref (msg ~> SN.autoland_complete)
  alt_set           <- deref (msg ~> SN.alt_set)
  alt_rate_set      <- deref (msg ~> SN.alt_rate_set)
  alt_set_valid     <- deref (msg ~> SN.alt_set_valid)
  heading_set       <- deref (msg ~> SN.heading_set)
  heading_set_valid <- deref (msg ~> SN.heading_set_valid)
  lat_set           <- deref (msg ~> SN.lat_set)
  lon_set           <- deref (msg ~> SN.lon_set)
  lat_lon_set_valid <- deref (msg ~> SN.lat_lon_set_valid)
  vel_x_set         <- deref (msg ~> SN.vel_x_set)
  vel_y_set         <- deref (msg ~> SN.vel_y_set)
  vel_set_valid     <- deref (msg ~> SN.vel_set_valid)
  v <- local (istruct
    [ NC.velocity_control   .= ival (fromInt8 vel_set_valid)
    , NC.vel_x_setpt        .= ival ((safeCast vel_x_set) / 1000.0)
    , NC.vel_y_setpt        .= ival ((safeCast vel_y_set) / 1000.0)
    , NC.position_control   .= ival (fromInt8 lat_lon_set_valid)
    , NC.lat_setpt          .= ival lat_set
    , NC.lon_setpt          .= ival lon_set
    , NC.altitude_control   .= ival (fromInt8 alt_set_valid)
    , NC.alt_setpt          .= ival ((safeCast alt_set) / 1000.0)
    , NC.alt_rate_setpt     .= ival ((safeCast alt_rate_set) / 1000.0)
    , NC.heading_control    .= ival (fromInt8 heading_set_valid)
    , NC.heading_setpt      .= ival ((safeCast heading_set) / 100.0)
    , NC.autoland_active    .= ival 0 -- (fromInt8 autoland_active)
    , NC.autoland_complete  .= ival 0 -- (fromInt8 autoland_complete)
    , NC.time .= ival now
    ])
  emit_ emitter (constRef v)

  where
  fromInt8 :: Sint8 -> E.EnableDisable
  fromInt8 i = (i ==? 0)?(E.none
               ,(i >? 0)?(E.enable
                ,E.disable))

--------------------------------------------------------------------------------

-- | Handle a 'COMPONENT_ARM_DISARM' command.
armDisarm :: SingI n
          => ChannelEmitter n (Struct "control_law_request")
          -> Uint32
          -> Ref s1 (Struct "command_long_msg")
          -> Ivory (ProcEffects cs r) ()
armDisarm creq_emitter now msg = do
  component <- deref (msg ~> CL.target_component)
  param1    <- deref (msg ~> CL.param1)
  when (component ==? (fromIntegral MC.id_SYSTEM_CONTROL)) $ do
    creq <- local $ istruct
      [ CR.set_armed    .= ival (param1 ==? 1.0)
      , CR.set_disarmed .= ival (param1 ==? 0.0)
      , CR.time         .= ival now
      ]
    emit_ creq_emitter (constRef creq)

--------------------------------------------------------------------------------

-- | Handle a 'COMMAND_LONG' subcommand.
handleCommandLong :: (GetAlloc eff ~ Scope s)
                  => Uint16
                  -> (Ref (Stack s) (Struct "command_long_msg") -> Ivory eff ())
                  -> Ref s1 (Struct "mavlink_receive_state")
                  -> Ivory eff ()
handleCommandLong cmd handler rxstate = handle go rxstate
  where
    go msg = do
      cmd_id <- deref (msg ~> CL.command)
      when (cmd_id ==? cmd) $
        handler msg

--------------------------------------------------------------------------------

-- | Handles a specific Mavlink message, where 'unpack' is a method of the
-- 'MavlinkUnpackageMsg'.
handle :: (GetAlloc eff ~ Scope s, MavlinkUnpackableMsg t, IvoryStruct t)
       => (Ref (Stack s) (Struct t) -> Ivory eff ())
       -> Ref s1 (Struct "mavlink_receive_state")
       -> Ivory eff ()
handle handler rxstate = do
  let (unpacker, msgid) = unpackMsg
  rxid <- deref (rxstate ~> R.msgid)
  msg  <- local (istruct [])
  when (rxid ==? msgid) $ do
    call_ unpacker msg (toCArray (constRef (rxstate ~> R.payload)))
    handler msg

--------------------------------------------------------------------------------

handlerModuleDefs :: ModuleDef
handlerModuleDefs = mapM_ depend mavlinkMessageModules

--------------------------------------------------------------------------------

