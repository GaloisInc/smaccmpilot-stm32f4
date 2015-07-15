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
         -> ChanInput (Array 8 (Stored Uint16))
         -> Tower e ()
ppmTower toppm tocc output = do
  ppm <- fmap toppm getEnv
  pulse_capt <- channel
  case ppm of
    PPM_Timer t p a i -> pulseCaptureTower tocc t p a i (fst pulse_capt)
    PPM_None -> error "cannot create SMACCMPilot.Hardware.PPM.ppmTower with PPM_None"

  ppmDecodeTower (snd pulse_capt) output

