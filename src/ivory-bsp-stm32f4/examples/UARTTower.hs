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

import Platforms
import qualified LEDTower

import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Regs

app :: forall p . (ColoredLEDs p) => Tower p ()
app = do
  LEDTower.blinkApp period [blue]

  ledctl <- channel

  ((istream :: ChannelSink 128 (Stored Uint8))
   ,(ostream :: ChannelSource 128 (Stored Uint8))) <- uartTower uart1 115200

  echoPrompt "hello world" ostream istream (src ledctl)

  task "settableLED" $ LEDTower.ledController [red] (snk ledctl)

  where
  period = 333
  -- On PX4FMU 1.x, these are the blue and red leds:
  red = redLED (undefined :: p)
  blue = blueLED (undefined :: p)

echoPrompt :: (SingI n, SingI m, SingI o)
           => String
           -> ChannelSource n (Stored Uint8)
           -> ChannelSink   m (Stored Uint8)
           -> ChannelSource o (Stored IBool)
           -> Tower p ()
echoPrompt greet ostream istream ledctlstream = task "echoprompt" $ do
  o <- withChannelEmitter  ostream "ostream"
  ledctl <- withChannelEmitter ledctlstream "ledctl"
  withStackSize 1024
  let puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str
      putc c = local (ival c) >>= \r -> emit_ o (constRef r)
      ledset b = local (ival b) >>= \r -> emit_ ledctl (constRef r)
  taskInit $ do
    puts (greet ++ "\n")
    puts "tower> "
  onChannelV istream "istream" $ \input -> do
    putc input -- echo to terminal
    cond_
      [ input `isChar` '1' ==>
          ledset true
      , input `isChar` '2' ==>
          ledset false
      , input `isChar` '\n' ==>
          puts "tower> "
      ]

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral (ord c))

