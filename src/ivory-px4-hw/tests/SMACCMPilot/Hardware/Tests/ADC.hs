{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.ADC (app) where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import SMACCMPilot.Hardware.ADC
import SMACCMPilot.Hardware.Platforms
import SMACCMPilot.Hardware.Serialize

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do

  adcOut <- channel
  a <- fmap (px4platform_adc . topx4) getEnv
  case a of
    Just adc -> adcTower (const adc) (fst adcOut)
    Nothing -> fail "adc unit test requires px4platform to have ADC"

  (uarto, _i, mon) <- px4ConsoleTower topx4
  monitor "console_uart" mon
  monitor "adcsender" $ do
    adcSender (snd adcOut) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

  where
  adcSender :: Sender e ('Stored IFloat)
  adcSender = sampleSender 'A' (Proxy :: Proxy 4)
