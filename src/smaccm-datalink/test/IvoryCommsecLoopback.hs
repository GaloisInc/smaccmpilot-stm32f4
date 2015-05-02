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
import Ivory.Tower.Compile
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.RingBuffer
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.UART

import qualified BSP.Tests.Platforms as BSP

import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Tower
import SMACCMPilot.Datalink.HXStream.Tower

main :: IO ()
main = towerCompile p (app fst snd)
  where
  p topts = do
    cfg <- getConfig topts parser
    return $ stm32FreeRTOS (BSP.testplatform_stm32 . fst) cfg

  parser = do
    f <- BSP.testPlatformParser
    s <- symmetricKeyParser
    return (f,s)

app :: (e -> BSP.TestPlatform)
    -> (e -> SymmetricKey)
    -> Tower e ()
app totp tosk = do
  tp <- fmap totp getEnv
  let cu = BSP.testplatform_uart tp
  (o, i) <- uartTower tocc (BSP.testUARTPeriph cu) (BSP.testUARTPins cu) 115200
  sk <- fmap tosk getEnv
  frame_loopback sk o i
  where
  tocc = BSP.testplatform_clockconfig . totp

frame_loopback :: SymmetricKey
               -> BackpressureTransmit HXCyphertext (Stored IBool)
               -> ChanOutput (Stored Uint8)
               -> Tower e ()
frame_loopback sk o i = do
  ct_buf_in <- channel
  ct_buf_out <- channel
  hxstreamDecodeTower "test" i (fst ct_buf_in)
  pt_out <- commsecDecodeTower "test" (symKeySaltArrayIval (sk_c2s sk))
                                      (snd ct_buf_out)
  ct_out <- commsecEncodeTower "test" (symKeySaltArrayIval (sk_s2c sk))
                                      pt_out
  hxstreamEncodeTower "test" ct_out o

  p <- period (Milliseconds 10)

  monitor "buffered_ctloopback" $ do
    (rb :: RingBuffer 4 CyphertextArray) <- monitorRingBuffer "loopback"
    handler (snd ct_buf_in) "ct_in" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
    handler p "periodic_pop" $ do
      e <- emitter (fst ct_buf_out) 1
      callback $ \_ -> do
        v <- local (iarray [])
        got <- ringbuffer_pop rb v
        when got $ do
          emit e (constRef v)

