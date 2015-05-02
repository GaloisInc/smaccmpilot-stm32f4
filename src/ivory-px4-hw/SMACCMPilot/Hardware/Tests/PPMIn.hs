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
import SMACCMPilot.Hardware.Tests.Platforms
import SMACCMPilot.Hardware.Tests.Serialize

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  (uarto, _i) <- px4ConsoleTower topx4

  ppmOut <- ppmTower (px4platform_ppm . topx4)
                     (px4platform_clockconfig . topx4)

  monitor "ppmsender" $ do
    ppmSender ppmOut uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

ppmSender :: Sender e (Array 8 (Stored Uint16))
ppmSender = sampleSender 'P' (Proxy :: Proxy 16)
