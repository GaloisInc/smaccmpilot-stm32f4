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
import qualified Ivory.OS.FreeRTOS.Task as F
import           Ivory.BSP.HWF4.USART

import qualified SMACCMPilot.Mavlink.Receive as R
import qualified SMACCMPilot.Flight.Types.DataRate as D

import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers

gcsReceiveTask :: (SingI n, SingI m)
               => MemArea (Struct "usart")
               -> ChannelSource n (Struct "gcsstream_timing")
               -> ChannelSource m (Struct "data_rate_state")
               -> Task ()
gcsReceiveTask usart_area s_src dr_src = do
  n <- freshname

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

  p <- withPeriod 1
  s_periods <- taskLocalInit "periods" defaultPeriods
  drInfo    <- taskLocal     "dropInfo"
  (buf :: Ref Global (Array 1 (Stored Uint8))) <- taskLocal     "buf"
  state     <- taskLocalInit "state" (istruct [ R.status .= ival R.status_IDLE ])

  taskInit $
    emit_ streamPeriodEmitter (constRef s_periods)

  onPeriod p $ \_now -> do
      -- XXX this task is totally invalid until we fix this to be part of the
      -- event loop
      n' <- call usartReadTimeout usart 1 (toCArray buf) 1
      unless (n' ==? 0) $ do
        b <- deref (buf ! 0)
        R.mavlinkReceiveByte state b
        s <- deref (state ~> R.status)
        cond_
          [ (s ==? R.status_GOTMSG) ==> do
              -- XXX We need to have a story for messages that are parsed correctly
              -- but are not recognized by the system---one could launch a DoS with
              -- those, too.
              t <- call F.getTimeMillis
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
    depend usartModule
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl handlerAux
    handlerModuleDefs

  where
  usart = addrOf usart_area

