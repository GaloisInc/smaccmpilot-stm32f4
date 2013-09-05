{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Motors.Task
  ( motorMixerTask
  , px4ioarMotorDecoder
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.ControlOutput as C
import qualified SMACCMPilot.Flight.Types.Motors        as M
import qualified SMACCMPilot.Flight.Types.FlightMode    as FM

import SMACCMPilot.Flight.Motors.Mixing

px4ioarMotorDecoder :: ConstRef s (Struct "motors")
                    -> Ivory (AllocEffects cs)
                          (ConstRef (Stack cs) (Array 4 (Stored IFloat)))
px4ioarMotorDecoder ms = do
  m1 <- deref (ms ~> M.frontleft)
  m2 <- deref (ms ~> M.frontright)
  m3 <- deref (ms ~> M.backright)
  m4 <- deref (ms ~> M.backleft)
  l <- local (iarray [ival m1, ival m2, ival m3, ival m4])
  return (constRef l)


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
  v <- local $ istruct zeroes
  return (constRef v)
  where
  zeroes = [ M.frontleft  .= ival 0
           , M.frontright .= ival 0
           , M.backleft   .= ival 0
           , M.backright  .= ival 0
           ]

