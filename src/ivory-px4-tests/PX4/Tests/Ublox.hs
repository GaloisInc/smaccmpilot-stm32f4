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
import qualified Ivory.HXStream as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.GPS.Types.Position as P
import SMACCMPilot.Hardware.GPS.Types.GPSFix
import SMACCMPilot.Hardware.GPS.UBlox

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt) -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  (_uarti,uarto) <- uartTower tocc (console px4platform)
                                115200 (Proxy :: Proxy 128)
  (gpsi, _gpso) <- uartTower tocc (gps px4platform)
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
  console = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform
  gps = px4platform_gps_device


positionSender :: ChanOutput (Struct "position")
               -> ChanInput (Stored Uint8)
               -> Monitor p ()
positionSender pos out = do
  (buf :: Ref Global (Array 46 (Stored Uint8))) <- state "pos_ser_buf"
  handler pos "position_serialize" $ do
    e <- emitter out (46*2+3)
    callback $ \p -> noReturn $ do
      f     <- deref (p ~> fix)
      t     <- deref (p ~> time)
      packInto_ buf 0 $ do
        mpackV ((f ==? fix_none) ? (0, (f ==? fix_2d) ? (2, 3 :: Uint8)))
        mpack  (p ~> num_sv)
        mpack  (p ~> dop)
        mpack  (p ~> lat)
        mpack  (p ~> lon)
        mpack  (p ~> alt)
        mpack  (p ~> vnorth)
        mpack  (p ~> veast)
        mpack  (p ~> vdown)
        mpack  (p ~> vground)
        mpack  (p ~> heading)
        mpackV (toIMicroseconds t)
      HX.encode tag (constRef buf) (emitV e)
  where
  tag = 112 -- 'p' for position

