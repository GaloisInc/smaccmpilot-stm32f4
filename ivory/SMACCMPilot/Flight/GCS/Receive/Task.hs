{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Tower

import qualified Ivory.OS.FreeRTOS as OS
import SMACCMPilot.Util.Periodic

import Ivory.BSP.HWF4.USART

import qualified SMACCMPilot.Mavlink.Receive as R

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import SMACCMPilot.Flight.GCS.Stream (defaultPeriods)

gcsReceiveTask :: MemArea (Struct "usart")
               -> Source (Struct "gcsstream_timing")
               -> String -> Task
gcsReceiveTask usart_area s_src uniquename =
  withSource "streamperiods" s_src $ \streamPeriodSource ->
  let tDef = proc ("gcsReceiveTaskDef" ++ uniquename) $ body $ do
        s_periods <- local defaultPeriods
        source streamPeriodSource (constRef s_periods)

        usart <- addrOf usart_area
        buf <- local (iarray []) ::
          Ivory (Top s) () (Ref (Stack s) (Array 1 (Stored Uint8)))
        state <- local (istruct [ R.status .= ival R.status_IDLE ])
        forever $ do
          n <- call usartRead usart (toCArray buf) 1
          ifte (n ==? 0)
            (return ())
            (do b <- deref (buf ! 0)
                R.mavlinkReceiveByte state b
                s <- deref (state ~> R.status)
                ifte (s ==? R.status_GOTMSG)
                  (forever (call_ OS.delay 1000)) -- trap for debugger.
                  (return ())
            )
      mDefs = do
        depend OS.taskModule
        depend usartModule
        defStruct (Proxy :: Proxy "mavlink_receive_state")
        incl tDef
  in task tDef mDefs

