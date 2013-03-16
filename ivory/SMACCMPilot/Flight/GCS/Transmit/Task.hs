
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Transmit.Task
  ( gcsTransmitTask
  ) where

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS
import SMACCMPilot.Util.Periodic

import SMACCMPilot.Flight.GCS.Transmit.MessageDriver
import SMACCMPilot.Flight.GCS.Transmit.USARTSender

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
        s_fm <- local (istruct [])
        periodic 1000 $ do
          sink flightModeSink s_fm
          call_ (sendHeartbeat chan1) s_fm

      mDefs = do
        depend OS.taskModule
        depend FM.flightModeTypeModule
        incl tDef
        mapM_ depend cmods
  
  in withModules cmods $ task tDef mDefs

