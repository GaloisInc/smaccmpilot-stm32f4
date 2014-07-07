{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module PX4.Tests.Ublox (app) where

import Ivory.Language
import Ivory.Serialize
import qualified Ivory.HXStream as HX

import Ivory.Tower

import Ivory.BSP.STM32.Driver.UART

import SMACCMPilot.Hardware.GPS.Types.Position as P
import SMACCMPilot.Hardware.GPS.Types.GPSFix
import SMACCMPilot.Hardware.GPS.UBlox

import PX4.Tests.Platforms

app :: forall p . (TestPlatform p) => Tower p ()
app = do
  boardInitializer
  (_uarti,uarto) <- uartTower (consoleUart (Proxy :: Proxy p))
                                115200 (Proxy :: Proxy 128)
  (gpsi, _gpso) <- uartTower (gpsUart (Proxy :: Proxy p))
                                38400 (Proxy :: Proxy 128)
  position <- channel
  ubloxGPSTower gpsi (src position)
  task "positionSender" $ do
    uartout <- withChannelEmitter uarto "uartout"
    positionSender (snk position) uartout

  towerDepends serializeModule
  towerModule  serializeModule

positionSender :: ChannelSink (Struct "position")
               -> ChannelEmitter (Stored Uint8)
               -> Task p ()
positionSender possink out = do
  pos <- withChannelEvent possink "position"
  (buf :: Ref Global (Array 46 (Stored Uint8))) <- taskLocal "pos_ser_buf"
  handle pos "position_serialize" $ \p -> noReturn $ do
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
    HX.encode tag (constRef buf) (emitV_ out)
  where
  tag = 112 -- 'p' for position

