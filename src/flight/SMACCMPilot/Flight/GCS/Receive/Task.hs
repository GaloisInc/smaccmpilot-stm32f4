{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import Prelude hiding (last, id)

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower
import qualified Ivory.OS.FreeRTOS.Task as F

import qualified SMACCMPilot.Mavlink.Receive as R
import qualified SMACCMPilot.Flight.Types.DataRate as D

import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers

import qualified SMACCMPilot.Flight.GCS.Commsec as C
import qualified Ivory.HXStream as H

import SMACCMPilot.Mavlink.Messages (mavlinkMessageModules)

--------------------------------------------------------------------------------

gcsReceiveTask :: (SingI n0, SingI n1, SingI n2)
               => ChannelSink   n0 (Stored Uint8) -- nn == 1024: uart buf
               -> ChannelSource n1 (Struct "gcsstream_timing")
               -> ChannelSource n2 (Struct "data_rate_state")
               -> Task p ()
gcsReceiveTask istream s_src dr_src = do

  withStackSize 1024
  streamPeriodEmitter <- withChannelEmitter s_src "streamperiods"

  drEmitter <- withChannelEmitter dr_src "data_rate_chan"

  s_periods <- taskLocalInit "periods" defaultPeriods
  drInfo    <- taskLocal     "dropInfo"
  state     <- taskLocalInit "state"
                 (istruct [ R.status .= ival R.status_IDLE ])
  hxState   <- taskLocal "hxState"

  taskInit $ do
    emit_ streamPeriodEmitter (constRef s_periods)
    H.emptyStreamState hxState

  onChannelV istream "istream" $ \b ->
    (call_ (parseMav drEmitter streamPeriodEmitter)
           state drInfo s_periods b)

  -- XXX
  -- onChannelV istream "istream" $ \b -> do
  --   -- hxstream state decode
  --   res  <- H.decodeSM hxState b
  --   -- check for overflow
  --   over <- hxState ~>* H.ovf
  --   cond_ [ over ==> H.emptyStreamState hxState
  --         , res  ==> do let buf = hxState ~> H.buf
  --                       C.decrypt C.uavCtx buf
  --                       arrayMap $ \ix -> deref (buf ! ix) >>= parseMav
  --         ]

  taskModuleDef $ do
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl handlerAux
    handlerModuleDefs
    defStruct (Proxy :: Proxy "hxstream_state")
    incl (H.decode :: Def ( '[ Ref s (Array 258 (Stored Uint8))
                             , Ref s (Struct "hxstream_state")
                             ] :-> Ix 258))
    depend C.commsecModule
    mapM_ depend mavlinkMessageModules

--------------------------------------------------------------------------------

handlerAux :: Def ('[ Ref s0 (Struct "mavlink_receive_state")
                    , Ref s1 (Struct "gcsstream_timing")
                    ] :-> ())
handlerAux = proc "gcsReceiveHandlerAux" $ \s streams -> body $
  runHandlers s
   [ handle paramRequestList
   , handle paramRequestRead
   , handle paramSet
   , handle (requestDatastream streams)
   , handle hilState
   ]
   where runHandlers s = mapM_ ($ s)

--------------------------------------------------------------------------------

parseMav :: (SingI n0, SingI n1)
         => ChannelEmitter n0 (Struct "data_rate_state")
         -> ChannelEmitter n1 (Struct "gcsstream_timing")
         -> Def ('[ Ref s0 (Struct "mavlink_receive_state")
                  , Ref s1 (Struct "data_rate_state")
                  , Ref s2 (Struct "gcsstream_timing")
                  , Uint8
                  ] :-> ())
parseMav drEmitter streamPeriodEmitter
  = proc "parseMav"
  $ \state drInfo s_periods b -> body $ do
  R.mavlinkReceiveByte state b
  s <- deref (state ~> R.status)

  cond_
    [ (s ==? R.status_GOTMSG) ==> do
        -- XXX We need to have a story for messages that are parsed
        -- correctly but are not recognized by the system---one could
        -- launch a DoS with those, too.
        t <- call F.getTimeMillis
        store (drInfo ~> D.lastSucc) t
        call_ handlerAux state s_periods
        R.mavlinkReceiveReset state
        -- XXX This should only be called if we got a request_data_stream
        -- msg.  Here it's called regardless of what incoming Mavlink
        -- message there is.
        emit_ streamPeriodEmitter (constRef s_periods)
    , (s ==? R.status_FAIL)   ==> do
        (drInfo ~> D.dropped) += 1
        store (state ~> R.status) R.status_IDLE
    ]
  emit_ drEmitter (constRef drInfo)
  retVoid

--------------------------------------------------------------------------------
