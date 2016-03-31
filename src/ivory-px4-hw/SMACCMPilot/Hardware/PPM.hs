{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM
  ( ppmTower
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Hardware.PPM.PulseCapture
import SMACCMPilot.Hardware.PPM.Decode

import Ivory.BSP.STM32.ClockConfig (ClockConfig(..))
import SMACCMPilot.Hardware.Platforms (PPM(..))

import SMACCMPilot.Comm.Ivory.Types (typeModules)
import Ivory.Serialize (serializeArtifacts, serializeModule)

ppmTower :: (e -> PPM)
         -> (e -> ClockConfig)
         -> ChanInput ('Struct "rc_input")
         -> Tower e ()
ppmTower toppm tocc output = do
  ppm <- fmap toppm getEnv
  pulse_capt <- channel
  case ppm of
    PPM_Timer t p a i -> pulseCaptureTower tocc t p a i (fst pulse_capt)
    _ -> error "SMACCMPilot.Hardware.PPM.ppmTower only supports PPM_Timer"


  ppmDecodeTower (snd pulse_capt) output

  mapM_ towerModule mods
  mapM_ towerDepends mods
  mapM_ towerArtifact serializeArtifacts
  where mods = serializeModule : typeModules

