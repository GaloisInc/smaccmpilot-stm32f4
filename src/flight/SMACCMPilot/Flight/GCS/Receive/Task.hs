{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import Prelude hiding (last, id)

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

import qualified SMACCMPilot.Mavlink.Receive as R
import qualified SMACCMPilot.Flight.Types.DataRate as D

import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers

gcsReceiveTask :: (SingI nn, SingI n, SingI m)
               => ChannelSink  nn (Stored Uint8)
               -> ChannelSource n (Struct "gcsstream_timing")
               -> ChannelSource m (Struct "data_rate_state")
               -> Task p ()
gcsReceiveTask istream s_src dr_src = do
  n <- freshname
  m <- withGetTimeMillis
  let handlerAux :: Def ('[ Ref s  (Struct "mavlink_receive_state")
                          , Ref s1 (Struct "gcsstream_timing")
                          ] :-> ())
      handlerAux = proc ("gcsReceiveHandlerAux" ++ n) $ \s streams -> body $
        runHandlers s
         [ handle paramRequestList
         , handle paramRequestRead
         , handle paramSet
         , handle (requestDatastream streams)
         , handle hilState
         ]
         where runHandlers s = mapM_ ((flip ($)) s)

  streamPeriodEmitter <- withChannelEmitter s_src "streamperiods"

  drEmitter <- withChannelEmitter dr_src "data_rate_chan"

  s_periods <- taskLocalInit "periods" defaultPeriods
  drInfo    <- taskLocal     "dropInfo"
  state     <- taskLocalInit "state" (istruct [ R.status .= ival R.status_IDLE ])

  taskInit $ emit_ streamPeriodEmitter (constRef s_periods)

  onChannelV istream "istream" $ \b -> do
    R.mavlinkReceiveByte state b
    s <- deref (state ~> R.status)
    cond_
      [ (s ==? R.status_GOTMSG) ==> do
          -- XXX We need to have a story for messages that are parsed correctly
          -- but are not recognized by the system---one could launch a DoS with
          -- those, too.
          t <- getTimeMillis m
          store (drInfo ~> D.lastSucc) t
          call_ handlerAux state s_periods
          R.mavlinkReceiveReset state
          -- XXX This should only be called if we got a request_data_stream msg.
          -- Here it's called regardless of what incoming Mavlink message there
          -- is.
          emit_ streamPeriodEmitter (constRef s_periods)
      , (s ==? R.status_FAIL)   ==> do
          (drInfo ~> D.dropped) += 1
          store (state ~> R.status) R.status_IDLE
      ]
    emit_ drEmitter (constRef drInfo)

  taskModuleDef $ do
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl handlerAux
    handlerModuleDefs

