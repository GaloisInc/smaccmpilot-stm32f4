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

import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX
import SMACCMPilot.Hardware.Tests.Platforms

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  (_i,uarto) <- px4ConsoleTower topx4

  ppmOut <- ppmTower (px4platform_ppm . topx4)
                     (px4platform_clockconfig . topx4)

  monitor "ppmsender" $ do
    ppmSender ppmOut uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

ppmSender :: ChanOutput (Array 8 (Stored Uint16))
               -> ChanInput  (Stored Uint8)
               -> Monitor e ()
ppmSender samples ostream = do
  (buf :: Ref Global (Array 16 (Stored Uint8))) <- state "ppm_ser_buf"
  handler samples "sample" $ do
    e <- emitter ostream (2*16 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 80 -- 'P' for ppm. lowercase p is position.
