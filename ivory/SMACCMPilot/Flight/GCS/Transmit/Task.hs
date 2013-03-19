
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS
import SMACCMPilot.Util.Periodic

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Transmit.USARTSender
import SMACCMPilot.Flight.GCS.Stream

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode as FM


import Ivory.BSP.HWF4.USART

sysid, compid :: Uint8
sysid = 1
compid = 0


gcsTransmitTask :: Sink (Struct "flightmode")
                -> String -> Task
gcsTransmitTask fm_sink uniquename =
  withSink "flightmode" fm_sink $ \flightModeSink ->

  let (chan1, cmods) = messageDriver (usartSender usart1 "usart1" sysid compid)

      tDef = proc ("gcsTransmitTaskDef" ++ uniquename) $ body $ do
        initTime <- call OS.getTimeMillis
        lastWake <- local (ival initTime)
        lastRun  <- local (ival initTime)

        s_periods  <- local defaultPeriods
        s_schedule <- local (istruct [])

        s_fm <- local (istruct [])
      --  s_servo <- local (istruct [])

        forever $ do
          now <- deref lastWake
          let onStream :: Label "gcsstream_timing" (Stored Uint32)
                       -> Ivory (Block s) () () -> Ivory s () ()
              onStream selector action = do
                last <- deref lastRun
                due <- streamDue (constRef s_periods) (constRef s_schedule) selector last now
                ifte due action (return ())

          onStream S.heartbeat $ do
            sink flightModeSink s_fm
            call_ (sendHeartbeat chan1) s_fm

       --   onStream S.servo_output_raw $ do
       --     sink servoSink s_servo
       --     call_ (sendServo chan1) s_fm

          nextWake <- nextDueTime (constRef s_periods) (constRef s_schedule) now
          dt <- assign (nextWake - now)
          store lastRun now
          call_ OS.delayUntil lastWake dt

      mDefs = do
        depend OS.taskModule
        depend FM.flightModeTypeModule
        incl tDef
        mapM_ depend cmods

  in withModules cmods $ task tDef mDefs

