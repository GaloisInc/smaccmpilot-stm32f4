{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.RingBuffer
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.UART

import qualified BSP.Tests.Platforms as BSP

import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Datalink.HXStream.Tower
import SMACCMPilot.Datalink.HXStream.Ivory (hxstreamModule)

main :: IO ()
main = compileTowerSTM32FreeRTOS BSP.testplatform_stm32 p (app id)
  where p topts = getConfig topts BSP.testPlatformParser

app :: (e -> BSP.TestPlatform)
    -> Tower e ()
app totp = do
  tp <- fmap totp getEnv
  let cu = BSP.testplatform_uart tp
  (o, i) <- uartTower tocc (BSP.testUARTPeriph cu) (BSP.testUARTPins cu) 115200
  frame_loopback o i
  where
  tocc = BSP.testplatform_clockconfig . totp

frame_loopback :: BackpressureTransmit HXCyphertext (Stored IBool)
               -> ChanOutput (Stored Uint8)
               -> Tower p ()
frame_loopback o i = do
  ctin <- channel
  ctout <- channel
  airDataDecodeTower "test" i (fst ctin)
  airDataEncodeTower "test" (snd ctout) o

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

