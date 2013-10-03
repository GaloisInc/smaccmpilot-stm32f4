{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import           Prelude hiding (last, id)

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

import qualified SMACCMPilot.Mavlink.Receive        as R
import qualified SMACCMPilot.Flight.Types.DataRate  as D
import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers
import           SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)
import qualified SMACCMPilot.Shared                 as S

--------------------------------------------------------------------------------

gcsReceiveTask :: (SingI n0, SingI n1, SingI n2, SingI n3)
               => ChannelSink   n0 S.MavLinkArray -- from decryptor
               -> ChannelSource n1 (Struct "gcsstream_timing")
               -> ChannelSource n2 (Struct "data_rate_state")
               -> ChannelSource n3 (Struct "hil_state_msg")
               -> Task p ()
gcsReceiveTask mavStream s_src dr_src hil_src = do
  m <- withGetTimeMillis
  hil_emitter <- withChannelEmitter hil_src "hil_src"

  withStackSize 1024
  streamPeriodEmitter <- withChannelEmitter s_src "streamperiods"

  drEmitter <- withChannelEmitter dr_src "data_rate_chan"

  s_periods <- taskLocalInit "periods" defaultPeriods
  drInfo    <- taskLocal     "dropInfo"
  state     <- taskLocalInit "state"
                 (istruct [ R.status .= ival R.status_IDLE ])

  taskInit $ emit_ streamPeriodEmitter (constRef s_periods)

  let parse = parseMav m hil_emitter drEmitter streamPeriodEmitter

  onChannel mavStream "mavStream" $ \ms -> do
    mav <- local (iarray [])
    refCopy mav ms
    call_ parse state drInfo s_periods (constRef mav)

  taskModuleDef $ do
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl (handlerAux hil_emitter)
    handlerModuleDefs
    mapM_ depend mavlinkMessageModules
    incl parse

--------------------------------------------------------------------------------

parseMav :: (SingI n0, SingI n1, SingI n2)
         => OSGetTimeMillis
         -> ChannelEmitter n2 (Struct "hil_state_msg")
         -> ChannelEmitter n0 (Struct "data_rate_state")
         -> ChannelEmitter n1 (Struct "gcsstream_timing")
         -> Def ('[ Ref s0 (Struct "mavlink_receive_state")
                  , Ref s0 (Struct "data_rate_state")
                  , Ref s0 (Struct "gcsstream_timing")
                  , ConstRef s1 S.MavLinkArray
                  ] :-> ())
parseMav m hil_emitter drEmitter streamPeriodEmitter
  = proc "parseMav"
  $ \state drInfo s_periods mav -> body $ do
  arrayMap $ \ix -> do b <- deref (mav ! ix)
                       R.mavlinkReceiveByte state b
  s <- deref (state ~> R.status)
  cond_
    [ (s ==? R.status_GOTMSG) ==> do
        -- XXX We need to have a story for messages that are parsed
        -- correctly but are not recognized by the system---one could
        -- launch a DoS with those, too.
        t <- getTimeMillis m
        store (drInfo ~> D.lastSucc) t
        call_ (handlerAux hil_emitter) state s_periods
        R.mavlinkReceiveReset state
        -- XXX This should only be called if we got a request_data_stream
        -- msg.  Here it's called regardless of what incoming Mavlink
        -- message there is.
        emit_ streamPeriodEmitter (constRef s_periods)
    , (s ==? R.status_FAIL)   ==> do
        (drInfo ~> D.dropped) += 1
        store (state ~> R.status) R.status_IDLE
    ]

  -- XXX data rate stuff
  -- emit_ drEmitter (constRef drInfo)
  retVoid

--------------------------------------------------------------------------------

handlerAux :: SingI n
  => ChannelEmitter n (Struct "hil_state_msg")
  -> Def ('[ Ref s0 (Struct "mavlink_receive_state")
           , Ref s1 (Struct "gcsstream_timing")
           ] :-> ())
handlerAux hil_emitter = proc "gcsReceiveHandlerAux" $ \s streams -> body $
  runHandlers s
   [ handle paramRequestList
   , handle paramRequestRead
   , handle paramSet
   , handle (requestDatastream streams)
   , handle (hilState hil_emitter)
   ]
   where runHandlers s = mapM_ ($ s)

--------------------------------------------------------------------------------
