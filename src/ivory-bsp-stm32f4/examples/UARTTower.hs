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
  LEDTower.blinkApp period [blue]

  (i :: Channel 128 (Stored Uint8)) <- channelWithSize
  (o :: Channel 128 (Stored Uint8)) <- channelWithSize

  ledctl <- channel

  uartTower uart1 115200   (snk i) (src o)
  echoPrompt "hello world" (src i) (snk o) (src ledctl)

  task "settableLED" $ LEDTower.ledController [red] (snk ledctl)

  where
  period = 333
  -- On PX4FMU 1.x, these are the blue and red leds:
  red = pinB15
  blue = pinB14

echoPrompt :: (SingI n, SingI m, SingI o)
           => String
           -> ChannelSource n (Stored Uint8)
           -> ChannelSink   m (Stored Uint8)
           -> ChannelSource o (Stored IBool)
           -> Tower ()
echoPrompt greet ostream istream ledctlstream = task "echoprompt" $ do
  o <- withChannelEmitter  ostream "ostream"
  i <- withChannelReceiver istream "istream"
  ledctl <- withChannelEmitter ledctlstream "ledctl"
  withStackSize 1024
  taskBody $ \sch -> do
    let puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str
        putc c = local (ival c) >>= \r -> emit_ o (constRef r)
        ledset b = local (ival b) >>= \r -> emit_ ledctl (constRef r)
    puts (greet ++ "\n")
    puts "tower> "
    eventLoop sch $ onChannel i $ \inref -> do
      input <- deref inref
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

