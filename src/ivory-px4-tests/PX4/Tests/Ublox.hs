{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module PX4.Tests.Ublox
  ( app
  , positionSender
  ) where

import Ivory.Language
import Ivory.Serialize
import qualified SMACCMPilot.Datalink.HXStream.Ivory as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.GPS.UBlox

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv

  let u = BSP.testplatform_uart (px4platform_testplatform px4platform)
  (_uarti, uarto) <- uartTower tocc (BSP.testUARTPeriph u) (BSP.testUARTPins u)
                               115200 (Proxy :: Proxy 256)

  let gps_periph = px4platform_gps_device px4platform
      gps_pins = px4platform_gps_pins px4platform
  (gpsi, _gpso) <- uartTower tocc gps_periph gps_pins
                                38400 (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (fst position)
  monitor "positionSender" $ do
    positionSender (snd position) uarto

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4


positionSender :: ChanOutput (Struct "position")
               -> ChanInput (Stored Uint8)
               -> Monitor p ()
positionSender pos out = do
  (buf :: Ref Global (Array 46 (Stored Uint8))) <- state "pos_ser_buf"
  handler pos "position_serialize" $ do
    e <- emitter out (46*2+3)
    callback $ \p -> noReturn $ do
      packInto_ buf 0 $ mpack p
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 112 -- 'p' for position

