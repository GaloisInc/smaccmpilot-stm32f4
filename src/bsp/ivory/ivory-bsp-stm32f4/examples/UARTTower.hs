{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module UARTTower where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW
import Ivory.BitData

import qualified LEDTower

import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Regs

app :: Tower ()
app = do
  LEDTower.blinkApp period leds

  (i :: Channel 128 (Stored Uint8)) <- channelWithSize
  (o :: Channel 128 (Stored Uint8)) <- channelWithSize

  uartTower uart1 115200 (snk i) (src o)
  echoPrompt             (src i) (snk o)

  where
  period = 333
  -- On PX4FMU 1.x, these are the blue and red leds:
  leds = [pinB14, pinB15]

echoPrompt :: (SingI n, SingI m)
           => ChannelSource n (Stored Uint8)
           -> ChannelSink  m (Stored Uint8)
           -> Tower ()
echoPrompt ostream istream = task "echoprompt" $ do
  o <- withChannelEmitter  ostream "ostream"
  i <- withChannelReceiver istream "istream"
  withStackSize 1024
  taskBody $ \sch -> do
    let puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str
        putc c = local (ival c) >>= \r -> emit_ sch o (constRef r)
    puts "Hello, World:\n"
    puts "tower>"
    eventLoop sch $ onChannel i $ \inref -> do
      input <- deref inref
      putc input
      when (input ==? (fromIntegral (ord '\n'))) $
        puts "tower>"



