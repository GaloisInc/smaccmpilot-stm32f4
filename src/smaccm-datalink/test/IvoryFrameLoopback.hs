{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Tower.Config
import Ivory.Tower.Compile
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.RingBuffer

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

import SMACCMPilot.Datalink.Ivory
import SMACCMPilot.Commsec.Sizes
import Ivory.HXStream (hxstreamModule)

main :: IO ()
main = towerCompile p (app id)
  where
  p topts = do
    cfg <- getConfig topts px4PlatformParser
    return $ stm32FreeRTOS px4platform_stm32config cfg

app :: (e -> PX4Platform F405.Interrupt)
    -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  (o,i) <- uartTower tocc (console px4platform) 115200 (Proxy :: Proxy 128)
  frame_loopback i o

  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4
  console = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform

frame_loopback :: ChanInput (Stored Uint8)
               -> ChanOutput (Stored Uint8)
               -> Tower p ()
frame_loopback o i = do
  ctin <- channel
  ctout <- channel
  decodeMonitor "test" i (fst ctin)
  encodeMonitor "test" (snd ctout) o

  p <- period (Milliseconds 10)

  monitor "buffered_ctloopback" $ do
    (rb :: RingBuffer 4 CyphertextArray) <- monitorRingBuffer "loopback"
    handler (snd ctin) "ct_in" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
    handler p "periodic_pop" $ do
      e <- emitter (fst ctout) 1
      callback $ \_ -> do
        v <- local (iarray [])
        got <- ringbuffer_pop rb v
        when got $ do
          emit e (constRef v)

  towerModule $ hxstreamModule

