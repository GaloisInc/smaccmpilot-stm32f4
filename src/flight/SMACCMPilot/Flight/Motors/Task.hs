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

import qualified SMACCMPilot.Flight.Types.Armed         as A
import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Motors        as M
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM

import SMACCMPilot.Flight.Motors.Mixing

motorMixerTask :: (SingI n, SingI m)
               => ChannelSink n (Struct "controloutput")
               -> DataSink (Stored A.ArmedMode)
               -> DataSink (Struct "flightmode")
               -> ChannelSource m (Struct "motors")
               -> Task p ()
motorMixerTask cs a fms ms = do
  armReader <- withDataReader a "armReader"
  fmReader  <- withDataReader fms "flightMode"
  motEmit   <- withChannelEmitter ms "motors"
  taskInit $ do
    d <- disabled
    emit_ motEmit d
  onChannel cs "control" $ \ctl -> do
    armedRef <- local (izero)
    fm       <- local (istruct [])
    readData armReader armedRef
    readData fmReader fm
    armed <- deref armedRef
    let output = emit_ motEmit
    ifte_ (armed ==? A.as_ARMED)
      (mixer ctl >>= output)
      (disabled >>= output)
  taskModuleDef $ do
    depend C.controlOutputTypeModule
    depend M.motorsTypeModule
    depend FM.flightModeTypeModule

disabled :: (GetAlloc eff ~ Scope cs) => Ivory eff (ConstRef (Stack cs) (Struct "motors"))
disabled = do
  v <- local $ istruct zeroes
  return (constRef v)
  where
  zeroes = [ M.frontleft  .= ival 0
           , M.frontright .= ival 0
           , M.backleft   .= ival 0
           , M.backright  .= ival 0
           ]

