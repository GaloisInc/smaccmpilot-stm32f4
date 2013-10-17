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

import qualified SMACCMPilot.Flight.Types.Armed                  as A
import qualified SMACCMPilot.Flight.Types.FlightMode             as FM
import qualified SMACCMPilot.Flight.Types.FlightModeData         as FM

import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Mavlink.Messages.RequestDataStream  as RDS
import qualified SMACCMPilot.Mavlink.Messages.ParamSet           as PS
import qualified SMACCMPilot.Mavlink.Messages.ParamRequestRead   as PRR

import qualified SMACCMPilot.Mavlink.Receive                     as R
import qualified SMACCMPilot.Mavlink.Messages.SetMode            as SM
import qualified SMACCMPilot.Mavlink.Messages.CommandLong        as CL
import           SMACCMPilot.Mavlink.Unpack

import qualified SMACCMPilot.Mavlink.Enums.MavComponent          as MC

import           SMACCMPilot.Param
import           SMACCMPilot.Flight.GCS.Stream (updateGCSStreamPeriods)

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
    name <- local (init_ParamString "")
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
  name  <- local (init_ParamString "")
  istr_from_sz name (constRef $ msg ~> PS.param_id)

  ixRef <- local (ival (0 :: Sint16))
  found <- call (paramIndexGetter getIndex) (constRef name) ixRef

  when found $ do
    ix  <- deref ixRef
    val <- deref (msg ~> PS.param_value)
    call_  setValue ix val
    emitV_ emitter ix

--------------------------------------------------------------------------------

requestDatastream :: Ref s1 (Struct "gcsstream_timing")
                  -> (ConstRef s1 (Struct "gcsstream_timing") -> Ivory eff ())
                  -> Ref s2 (Struct "request_data_stream_msg")
                  -> Ivory eff ()
requestDatastream streamperiods send msg = do
  rsid   <- deref (msg ~> RDS.req_stream_id)
  enable <- deref (msg ~> RDS.start_stop)
  rate   <- deref (msg ~> RDS.req_message_rate)
  updateGCSStreamPeriods streamperiods rsid (enable >? 0) rate
  send (constRef streamperiods)

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

-- | Return true if a flight mode is valid and the corresponding mode.
isValidMode :: Uint8 -> Ivory (ProcEffects cs r) IBool
isValidMode x = go FM.flightModes
                    -- fm here should never be used.
  where go []     = return false
        go (y:ys) = ifte (x ==? FM.fromFlightMode y)
                      (return true)
                      (go ys)

-- | Returns the corresponding flight mode.  Invariant: input is a valid
-- flightmode.
getMode :: Uint8 -> Ivory (ProcEffects cs r) FM.FlightMode
getMode x = go FM.flightModes
                    -- fm here should never be used.
  where go []     = return FM.flightModeStabilize -- Shouldn't happen.
        go (y:ys) = ifte (x ==? FM.fromFlightMode y)
                      (return y)
                      (go ys)

-- | Set the flight mode.
setMode :: DataWriter (Struct "flightmode")
        -> Uint32
        -> Ref s2 (Struct "set_mode_msg")
        -> Ivory (ProcEffects cs r) ()
setMode fm_writer now msg = do
  fm <- local izero
  base_mode <- deref (msg ~> SM.base_mode)

  when (base_mode .& customModeEnabled /=? 0) $ do
    mode32 <- deref (msg ~> SM.custom_mode)
    -- Assume, for now, that we're only using 8 bits of the mode.
    mode   <- assign (bitCast mode32)
    valid  <- isValidMode mode
    mode'  <- getMode mode
    when valid $ do
      store (fm ~> FM.mode)  mode'
      store (fm ~> FM.time)  now
      writeData fm_writer (constRef fm)

--------------------------------------------------------------------------------

-- | Handle a 'COMPONENT_ARM_DISARM' command.
armDisarm :: SingI n
          => ChannelEmitter n (Stored A.ArmedMode)
          -> Ref s1 (Struct "command_long_msg")
          -> Ivory (ProcEffects cs r) ()
armDisarm arm_emitter msg = do
  component <- deref (msg ~> CL.target_component)
  param1    <- deref (msg ~> CL.param1)

  when (component ==? (fromIntegral MC.id_SYSTEM_CONTROL)) $ do
    cond_
      -- Float comparison should be ok(?) since they're encoded ints and there's
      -- no arithmetic on them.
      [ param1 ==? 0.0 ==> emitV_ arm_emitter A.as_DISARMED
      , param1 ==? 1.0 ==> emitV_ arm_emitter A.as_ARMED
      ]
  return ()

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

