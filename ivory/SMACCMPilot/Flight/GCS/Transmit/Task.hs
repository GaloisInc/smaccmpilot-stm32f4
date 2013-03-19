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

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Transmit.USARTSender
import SMACCMPilot.Flight.GCS.Stream

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode as FM

sysid, compid :: Uint8
sysid = 1
compid = 0


gcsTransmitTask :: MemArea (Struct "usart")
                -> Sink (Struct "gcsstream_timing")
                -> Sink (Struct "flightmode")
                -> String -> Task
gcsTransmitTask usart s_sink fm_sink uniquename =
  withSink "streamperiods" s_sink $ \streamPeriodSink ->
  withSink "flightmode" fm_sink $ \flightModeSink ->

  let (chan1, cmods) = messageDriver (usartSender usart uniquename sysid compid)

      tDef = proc ("gcsTransmitTaskDef" ++ uniquename) $ body $ do
        initTime <- call OS.getTimeMillis
        lastWake <- local (ival initTime)
        lastRun  <- local (ival initTime)

        s_newperiods  <- local (istruct [])
        s_periods     <- local (istruct [])
        s_schedule    <- local (istruct [])

        s_fm <- local (istruct [])
      --  s_servo <- local (istruct [])

        forever $ do
          now <- deref lastWake

          -- Update periods, adding streams to schedule if they are now enabled
          sink streamPeriodSink s_newperiods
          setNewPeriods (constRef s_newperiods) s_periods s_schedule now

          -- Handler for all streams - if due, run action, then update schedule
          let onStream :: Label "gcsstream_timing" (Stored Uint32)
                       -> Ivory (Block s) () () -> Ivory s () ()
              onStream selector action = do
                last <- deref lastRun
                due <- streamDue (constRef s_periods) (constRef s_schedule) selector last now
                ifte due
                  (do action
                      setNextTime (constRef s_periods) s_schedule selector now)
                  (return ())

          onStream S.heartbeat $ do
            sink flightModeSink s_fm
            call_ (sendHeartbeat chan1) s_fm

       --   onStream S.servo_output_raw $ do
       --     sink servoSink s_servo
       --     call_ (sendServo chan1) s_fm

          -- Calculate next scheduler time
          nextWake <- nextDueTime (constRef s_periods) (constRef s_schedule) now
          dt <- assign (nextWake - now)
          store lastRun now
          -- Cap sleep time at 500ms so streamPeriodSink can be serviced
          -- periodically
          call_ OS.delayUntil lastWake
            ((dt >? 500) ? (500, dt))


      mDefs = do
        depend OS.taskModule
        depend FM.flightModeTypeModule
        depend S.gcsStreamTimingTypeModule
        incl tDef
        mapM_ depend cmods

  in withModules cmods $ task tDef mDefs

