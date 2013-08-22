{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Motors.Task
  ( motorMixerTask
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Motors        as M
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM

import SMACCMPilot.Flight.Motors.Mixing

motorMixerTask :: (SingI n, SingI m)
               => ChannelSink n (Struct "controloutput")
               -> DataSink (Struct "flightmode")
               -> ChannelSource m (Struct "motors")
               -> Task p ()
motorMixerTask cs fms ms = do
  fmReader  <- withDataReader fms "flightMode"
  motEmit   <- withChannelEmitter ms "motors"
  taskInit $ do
    d <- disabled
    emit_ motEmit d
  onChannel cs "control" $ \ctl -> do
    fm <- local (istruct [])
    readData fmReader fm
    armed <- deref (fm ~> FM.armed)
    let output = emit_ motEmit
    ifte_ armed
      ((mixer ctl) >>= output)
      (disabled >>= output)
  taskModuleDef $ do
    depend C.controlOutputTypeModule
    depend M.motorsTypeModule
    depend FM.flightModeTypeModule

disabled :: (GetAlloc eff ~ Scope cs) => Ivory eff (ConstRef (Stack cs) (Struct "motors"))
disabled = do
  v <- local $ istruct [ M.ms .= iarray [ ival 0, ival 0, ival 0, ival 0 ] ]
  return (constRef v)

