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
  ctlRxer   <- withChannelReceiver cs "ctlOut"
  fmReader  <- withDataReader fms "flightMode"
  motEmit   <- withChannelEmitter ms "motors"
  millis    <- withGetTimeMillis
  taskInit $ do
    t <- getTimeMillis millis
    d <- disabled t
    emit_ motEmit d
  onChannel ctlRxer $ \ctl -> do
    fm <- local (istruct [])
    readData fmReader fm
    t <- getTimeMillis millis
    armed <- deref (fm ~> FM.armed)
    let output = emit_ motEmit
    ifte_ armed
      ((mixer ctl t) >>= output)
      ((disabled t) >>= output)
  taskModuleDef $ do
    depend C.controlOutputTypeModule
    depend M.motorsTypeModule
    depend FM.flightModeTypeModule

disabled :: (GetAlloc eff ~ Scope cs) => Uint32 -> Ivory eff (ConstRef (Stack cs) (Struct "motors"))
disabled t = do
  v <- local $ istruct
                 [ M.motor1 .= ival 0
                 , M.motor2 .= ival 0
                 , M.motor3 .= ival 0
                 , M.motor4 .= ival 0
                 , M.time   .= ival t
                 ]
  return (constRef v)

