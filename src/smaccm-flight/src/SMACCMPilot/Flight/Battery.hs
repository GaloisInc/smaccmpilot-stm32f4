{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Battery
  ( batteryTower
  ) where

import           Ivory.Tower
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle
import           SMACCMPilot.Flight.Platform
import           SMACCMPilot.Hardware.ADC

batteryTower :: (e -> FlightPlatform)
             -> ControllableVehicleAttrs Attr
             -> Tower e ()
batteryTower tofp attrs = do
  vbatt <- channel
  fp <- fmap tofp getEnv
  case fp_vbatt_adc fp of
    Nothing -> return ()
    Just adc -> do
      adcTower (const adc) (fst vbatt)
  attrProxy (batteryVoltage attrs) (snd vbatt)


