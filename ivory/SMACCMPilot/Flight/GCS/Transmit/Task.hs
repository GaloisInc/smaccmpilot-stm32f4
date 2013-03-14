
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

import qualified SMACCMPilot.Flight.Types.FlightMode as FM

gcsTransmitTask :: Sink (Struct "flightmode")
                -> String -> Task
gcsTransmitTask fm_sink uniquename =
  withSink "flightmode" fm_sink $ \flightModeSink ->
  let tDef = proc ("gcsTransmitTaskDef" ++ uniquename) $ body $ do
        s_fm <- local (istruct [])
        periodic 250 $ do
          sink flightModeSink s_fm
          -- call_ (sendHeartbeat chan) s_fm

      mDefs = do
        depend OS.taskModule
        depend FM.flightModeTypeModule
        incl tDef
        -- depend < generated message drivers >
      -- chan = messageDriver (mavlinkSender < something >)
  in withModules [{- < generated message drivers > -}] $ task tDef mDefs

