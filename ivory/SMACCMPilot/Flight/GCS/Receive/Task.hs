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

import qualified Ivory.OS.FreeRTOS as OS

import qualified SMACCMPilot.Mavlink.Receive as R

import           SMACCMPilot.Flight.GCS.Stream (defaultPeriods)
import           SMACCMPilot.Flight.GCS.Receive.Handlers

gcsReceiveTask :: MemArea (Struct "usart")
               -> Source (Struct "gcsstream_timing")
               -> String -> Task
gcsReceiveTask usart_area s_src uniquename =
  withSource "streamperiods" s_src $ \streamPeriodSource ->
  let tDef = proc ("gcsReceiveTaskDef" ++ uniquename) $ body $ do
        s_periods <- local defaultPeriods
        source streamPeriodSource (constRef s_periods)

        usart <- addrOf usart_area
        buf <- local (iarray [] :: Init (Array 1 (Stored Uint8)))
        state <- local (istruct [ R.status .= ival R.status_IDLE ])
        forever $ do
          n <- call (direct usartRead usart (toCArray buf) 1)
          ifte (n ==? 0) (return ()) $ do
            b <- deref (buf ! 0)
            R.mavlinkReceiveByte state b
            s <- deref (state ~> R.status)
            ifte (s /=? R.status_GOTMSG) (return ()) $ do
              call (direct_ handlerAux state s_periods)
              R.mavlinkReceiveReset state
              source streamPeriodSource (constRef s_periods)

      handlerAux :: Def ('[ Ref s (Struct "mavlink_receive_state")
                          , Ref s1 (Struct "gcsstream_timing") ] :-> ())
      handlerAux = proc ("gcsReceiveHandlerAux" ++ uniquename) $ \s streams -> body $
        runHandlers s
         [ handle paramRequestList
         , handle paramRequestRead
         , handle paramSet
         , handle (requestDatastream streams)
         , handle hilState
         ]
      runHandlers s = mapM_ ((flip($)) s)
      mDefs = do
        depend OS.taskModule
        depend usartModule
        defStruct (Proxy :: Proxy "mavlink_receive_state")
        incl tDef
        incl handlerAux
        handlerModuleDefs
  in task tDef mDefs

