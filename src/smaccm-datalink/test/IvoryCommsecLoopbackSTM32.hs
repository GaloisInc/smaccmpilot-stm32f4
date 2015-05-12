module Main where

import Ivory.Tower
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import Ivory.BSP.STM32.Driver.UART

import qualified BSP.Tests.Platforms as BSP

import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Datalink.Loopback

main :: IO ()
main = compileTowerSTM32FreeRTOS (BSP.testplatform_stm32 . fst) p (app fst snd)
  where
  p topts = getConfig topts parser
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
