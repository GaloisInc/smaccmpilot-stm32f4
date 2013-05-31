{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import Prelude hiding (last, id)

import           Ivory.Language
import           Ivory.Tower
import           Ivory.BSP.HWF4.USART

import qualified SMACCMPilot.Mavlink.Receive as R

import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers

gcsReceiveTask :: MemArea (Struct "usart")
               -> ChannelSource (Struct "gcsstream_timing")
               -> Task ()
gcsReceiveTask usart_area s_src = do
  n <- freshname
  streamPeriodEmitter <- withChannelEmitter s_src "streamperiods"

  let handlerAux :: Def ('[ Ref s (Struct "mavlink_receive_state")
                      , Ref s1 (Struct "gcsstream_timing") ] :-> ())
      handlerAux = proc ("gcsReceiveHandlerAux" ++ n) $ \s streams -> body $
        runHandlers s
         [ handle paramRequestList
         , handle paramRequestRead
         , handle paramSet
         , handle (requestDatastream streams)
         , handle hilState
         ]
         where runHandlers s = mapM_ ((flip($)) s)
  p <- withPeriod 1
  taskBody $ \schedule -> do
    s_periods <- local defaultPeriods
    emit schedule streamPeriodEmitter (constRef s_periods)

    usart <- addrOf usart_area
    buf <- local (iarray [] :: Init (Array 1 (Stored Uint8)))
    state <- local (istruct [ R.status .= ival R.status_IDLE ])
    eventLoop schedule $ onTimer p $ \_now -> do
      -- XXX this task is totally invalid until we fix this to be part of the
      -- event loop
      n' <- call usartReadTimeout usart 1 (toCArray buf) 1
      ifte (n' ==? 0) (return ()) $ do
        b <- deref (buf ! 0)
        R.mavlinkReceiveByte state b
        s <- deref (state ~> R.status)
        ifte (s /=? R.status_GOTMSG) (return ()) $ do
          call_ handlerAux state s_periods
          R.mavlinkReceiveReset state
          emit schedule streamPeriodEmitter (constRef s_periods)


  taskModuleDef $ \_sch -> do
    depend usartModule
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl handlerAux
    handlerModuleDefs

