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

import qualified SMACCMPilot.Flight.Types.ControlLaw    as CL
import qualified SMACCMPilot.Flight.Types.ArmedMode     as A
import qualified SMACCMPilot.Flight.Types.Motors        as M

import SMACCMPilot.Flight.Motors.Mixing

motorMixerTask :: (SingI n, SingI m)
               => ChannelSink n (Struct "controloutput")
               -> DataSink (Struct "control_law")
               -> ChannelSource m (Struct "motors")
               -> Task p ()
motorMixerTask cs clsnk ms = do
  clReader  <- withDataReader clsnk "controllaw"
  motEmit   <- withChannelEmitter ms "motors"
  taskInit $ do
    d <- disabled
    emit_ motEmit d
  onChannel cs "control" $ \ctl -> do
    cl <- local (istruct [])
    readData clReader cl
    armed <- deref (cl ~> CL.armed_mode)
    let output = emit_ motEmit
    ifte_ (armed ==? A.armed)
      (mixer ctl >>= output)
      (disabled >>= output)
  taskModuleDef $ do
    depend M.motorsTypeModule

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

