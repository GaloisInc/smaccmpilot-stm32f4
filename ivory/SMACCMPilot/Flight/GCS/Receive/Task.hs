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

gcsReceiveTask :: String -> MemArea (Struct "usart")
               -> EventSource (Struct "gcsstream_timing")
               -> Task ()
gcsReceiveTask usartname usart_area s_src = do
  n <- freshname
  streamPeriodEmitter <- withEventEmitter s_src "streamperiods"

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

  taskBody $ proc ("gcsReceiveTaskDef_" ++ usartname ++ n) $ body $ do
    s_periods <- local defaultPeriods
    emit streamPeriodEmitter (constRef s_periods)

    usart <- addrOf usart_area
    buf <- local (iarray [] :: Init (Array 1 (Stored Uint8)))
    state <- local (istruct [ R.status .= ival R.status_IDLE ])
    forever $ do
      n <- call usartRead usart (toCArray buf) 1  -- at some point, turn this into event driven
      ifte (n ==? 0) (return ()) $ do
        b <- deref (buf ! 0)
        R.mavlinkReceiveByte state b
        s <- deref (state ~> R.status)
        ifte (s /=? R.status_GOTMSG) (return ()) $ do
          call_ handlerAux state s_periods
          R.mavlinkReceiveReset state
          emit streamPeriodEmitter (constRef s_periods)


  taskModuleDef $ do
    depend usartModule
    defStruct (Proxy :: Proxy "mavlink_receive_state")
    incl handlerAux
    handlerModuleDefs

