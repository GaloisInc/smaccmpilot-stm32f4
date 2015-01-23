{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PX4.Tests.HMC5883L (hmc5883lSender, hmc5883lctl, app) where

import Ivory.Language
import Ivory.Serialize

import Ivory.Tower

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import SMACCMPilot.Hardware.HMC5883L

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  towerModule  hmc5883lTypesModule
  towerDepends hmc5883lTypesModule

  let hmc = px4platform_hmc5883_device px4platform
  (req, res, _ready) <- i2cTower tocc
                         (hmc5883device_periph hmc)
                         (hmc5883device_sda    hmc)
                         (hmc5883device_scl    hmc)

  samples <- hmc5883lctl req res (hmc5883device_addr hmc)

  let u = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  (_uarti,uarto) <- uartTower tocc (u px4platform) 115200 (Proxy :: Proxy 128)

  monitor "hmc5883lsender" $ do
    hmc5883lSender samples uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4

hmc5883lSender :: ChanOutput (Struct "hmc5883l_sample")
               -> ChanInput  (Stored Uint8)
               -> Monitor e ()
hmc5883lSender samples ostream = do
  (buf :: Ref Global (Array 22 (Stored Uint8))) <- state "hmc5883l_ser_buf"
  handler samples "sample" $ do
    e <- emitter ostream (2*18 + 3) -- twice buf size plus tag and two fbos
    callback $ \s -> noReturn $ do
      packInto_ buf 0 $ mpack s
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 99 -- 'c' for compass
