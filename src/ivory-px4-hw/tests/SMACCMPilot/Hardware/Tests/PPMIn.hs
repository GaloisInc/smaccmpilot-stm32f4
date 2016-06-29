{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.PPMIn (app) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import SMACCMPilot.Hardware.PPM
import SMACCMPilot.Hardware.PX4IO
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Serialize
import qualified SMACCMPilot.Comm.Ivory.Types.Px4ioState as PX4IO

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  (uarto, _i, mon) <- px4ConsoleTower topx4
  monitor "consule_uart" mon

  ppmOut <- channel
  env <- fmap topx4 getEnv
  case px4platform_ppm env of
    PPM_None -> case px4platform_px4io env of
      PX4IO_None -> error "SMACCMPilot.Hardware.Tests.PPMIn requires either valid PPM or PX4IO hardware"
      PX4IO_Serial u p c -> px4ioPPMInDriver u p c ppmOut

    _ -> nativePPMDriver ppmOut

  monitor "ppmsender" $ do
    ppmSender (snd ppmOut) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

  where
  ppmSender :: Sender e ('Struct "rc_input")
  ppmSender = sampleSender 'P' (Proxy :: Proxy 21)

  nativePPMDriver ppmOut = do
    ppmTower (px4platform_ppm . topx4)
             (px4platform_clockconfig . topx4)
             (fst ppmOut)

  px4ioPPMInDriver u p c ppmOut = do
    dummy_cl <- channel
    dummy_motors <- channel
    px4io_state <- channel
    px4ioTower
         (px4platform_clockconfig . topx4)
         u p c
         (snd dummy_cl)
         (snd dummy_motors)
         (fst px4io_state)

    monitor "px4io_get_rcin" $ do
      handler (snd px4io_state) "" $ do
        e <- emitter (fst ppmOut) 1
        callback $ \ s -> emit e (s ~> PX4IO.rc_in)
