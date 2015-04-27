{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.PPM
  ( ppmTower
  ) where

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Hardware.PPM.PulseCapture
import SMACCMPilot.Hardware.PPM.Decode

import Ivory.BSP.STM32.ClockConfig (ClockConfig(..))
import SMACCMPilot.Hardware.Tests.Platforms (PPM(..))

ppmTower :: (e -> PPM)
         -> (e -> ClockConfig)
         -> Tower e (ChanOutput (Array 8 (Stored Uint16)))
ppmTower toppm tocc = do
  ppm <- fmap toppm getEnv
  pulse_capt <- channel
  case ppm of
    PPM_Timer t p a i -> pulseCaptureTower tocc t p a i (fst pulse_capt)
    PPM_None -> error "cannot create SMACCMPilot.Hardware.PPM.ppmTower with PPM_None"

  output <- channel
  ppmDecodeTower (snd pulse_capt) (fst output)
  return (snd output)

