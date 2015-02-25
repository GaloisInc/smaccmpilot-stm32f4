{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.Baro (ms5611Sender, ms5611I2CSensorManager, app) where

import Ivory.Language
import Ivory.Serialize
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C

import SMACCMPilot.Hardware.MS5611

import PX4.Tests.Platforms

app :: (e -> PX4Platform) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  measurements <- channel

  case px4platform_baro px4platform of
    Baro_MS5611_I2C m -> ms5611_i2c_app topx4 m (fst measurements)
    _ -> error "baro app body case statement left partially implemented"


  (_uarti, uarto) <- px4ConsoleTower topx4
  monitor "ms5611sender" $ do
    ms5611Sender (snd measurements) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts


ms5611_i2c_app :: (e -> PX4Platform)
               -> MS5611_I2C
               -> ChanInput (Struct "ms5611_measurement")
               -> Tower e ()
ms5611_i2c_app topx4 ms5611 meas = do
  (req, res, ready) <- i2cTower (px4platform_clockconfig . topx4)
                         (ms5611_i2c_periph ms5611)
                         (ms5611_i2c_sda ms5611)
                         (ms5611_i2c_scl ms5611)
  ms5611I2CSensorManager req res ready meas (ms5611_i2c_addr ms5611)



ms5611Sender :: ChanOutput (Struct "ms5611_measurement")
             -> ChanInput (Stored Uint8)
             -> Monitor e ()
ms5611Sender meas out = do
  (buf :: Ref Global (Array 18 (Stored Uint8))) <- state "ms5611_ser_buf"
  handler meas "measurement" $ do
    e <- emitter out (2*18 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto buf 0 s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 98 -- 'b' for barometer
