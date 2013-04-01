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
import qualified Ivory.OS.FreeRTOS.Task as Task

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Transmit.USARTSender
import SMACCMPilot.Flight.GCS.Stream

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import qualified SMACCMPilot.Flight.Types.FlightMode as FM

sysid, compid :: Uint8
sysid = 1
compid = 0

gcsTransmitTask :: MemArea (Struct "usart")
                -> EventSink (Struct "gcsstream_timing")
                -> DataSink (Struct "flightmode")
                -> DataSink (Struct "sensors_result")
                -> DataSink (Struct "position_result")
                -> DataSink (Struct "controloutput")
                -> DataSink (Struct "servos")
                -> String -> Task
gcsTransmitTask usart sp_sink fm_sink se_sink ps_sink ct_sink sr_sink uniquename =
  withPolledEvent "streamperiods" sp_sink $ \streamPeriodSink ->
  withDataSink "flightmode" fm_sink $ \flightModeSink ->
  withDataSink "sensors"    se_sink $ \sensorsSink    ->
  withDataSink "position"   ps_sink $ \positionSink   ->
  withDataSink "control"    ct_sink $ \controlSink    ->
  withDataSink "servos"     sr_sink $ \servosSink     ->

  let (chan1, cmods) = messageDriver (usartSender usart uniquename sysid compid)

      tDef = proc ("gcsTransmitTaskDef" ++ uniquename) $ body $ do
        initTime <- call Task.getTimeMillis
        lastWake <- local (ival initTime)
        lastRun  <- local (ival initTime)

        s_newperiods  <- local (istruct [])
        s_periods     <- local (istruct [])
        s_schedule    <- local (istruct [])

        s_fm   <- local (istruct [])
        s_sens <- local (istruct [])
        s_pos  <- local (istruct [])
        s_ctl  <- local (istruct [])
        s_serv <- local (istruct [])

        forever $ do
          now <- deref lastWake

          -- Update periods, adding streams to schedule if they are now enabled
          got <- poll streamPeriodSink s_newperiods 0
          ifte got
            (setNewPeriods (constRef s_newperiods) s_periods s_schedule now)
            (return ())

          -- Handler for all streams - if due, run action, then update schedule
          let onStream :: Label "gcsstream_timing" (Stored Uint32)
                       -> Ivory eff () -> Ivory eff ()
              onStream selector action = do
                last <- deref lastRun
                due <- streamDue (constRef s_periods) (constRef s_schedule) selector last now
                ifte due
                  (do action
                      setNextTime (constRef s_periods) s_schedule selector now)
                  (return ())

          onStream S.heartbeat $ do
            dataSink flightModeSink s_fm
            call_ (sendHeartbeat chan1) s_fm

          onStream S.servo_output_raw $ do
            dataSink servosSink s_serv
            dataSink controlSink s_ctl
            call_ (sendServoOutputRaw chan1) s_serv s_ctl

          onStream S.attitude $ do
            dataSink sensorsSink s_sens
            call_ (sendAttitude chan1) s_sens

          onStream S.gps_raw_int $ do
            dataSink positionSink s_pos
            call_ (sendGpsRawInt chan1) s_pos

          onStream S.vfr_hud $ do
            dataSink positionSink s_pos
            dataSink controlSink s_ctl
            dataSink sensorsSink s_sens
            call_ (sendVfrHud chan1) s_pos s_ctl s_sens

          onStream S.global_position_int $ do
            dataSink positionSink s_pos
            dataSink sensorsSink s_sens
            call_ (sendGlobalPositionInt chan1) s_pos s_sens

          onStream S.params $ do
            -- XXX our whole story for params is broken
            return ()

          -- Calculate next scheduler time
          nextWake <- nextDueTime (constRef s_periods) (constRef s_schedule) now
          dt <- assign (nextWake - now)
          store lastRun now
          -- Cap sleep time at 500ms so streamPeriodSink can be serviced
          -- periodically
          call_ Task.delayUntil lastWake
            ((dt >? 500) ? (500, dt))


      mDefs = do
        depend Task.taskModule
        depend FM.flightModeTypeModule
        depend S.gcsStreamTimingTypeModule
        incl tDef
        mapM_ depend cmods

  in withModules cmods $ withStackSize 512 $ task tDef mDefs

