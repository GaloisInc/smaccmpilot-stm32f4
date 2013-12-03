{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
import Ivory.BSP.STM32F4.RCC

app :: forall p . (ColoredLEDs p, BoardHSE p) => Tower p ()
app = do
  LEDTower.blinkApp period [blue]

  redledctl <- channel

  (istream, ostream) <- uartTower uart5 115200

  echoPrompt "hello world" ostream istream (src redledctl)

  task "settableLED" $ LEDTower.ledController [red] (snk redledctl)

  where
  period = 333
  -- red and blue leds provided through ColoredLEDs typeclass:
  red = redLED (Proxy :: Proxy p)
  blue = blueLED (Proxy :: Proxy p)

echoPrompt :: (SingI n)
           => String
           -> ChannelSource 128 (Stored Uint8)
           -> ChannelSink   128 (Stored Uint8)
           -> ChannelSource n (Stored IBool)
           -> Tower p ()
echoPrompt greet ostream istream ledctlstream = task "echoprompt" $ do
  o      <- withChannelEmitter ostream      "ostream"
  ledctl <- withChannelEmitter ledctlstream "ledctl"

  let puts :: (GetAlloc eff ~ Scope cs) => String -> Ivory eff ()
      puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ Scope cs) => Uint8 -> Ivory eff ()
      putc = emitV_ o

  taskInit $ do
    puts (greet ++ "\n")
    puts "tower> "
  onChannelV istream "istream" $ \input -> do
    putc input -- echo to terminal
    cond_
      [ input `isChar` '1' ==>
          emitV_ ledctl true
      , input `isChar` '2' ==>
          emitV_ ledctl false
      , input `isChar` '\n' ==>
          puts "tower> "
      ]

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral (ord c))

