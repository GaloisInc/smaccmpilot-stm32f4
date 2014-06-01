{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module UARTTower where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Platforms
import LEDTower

import Ivory.BSP.STM32F405.Init
import Ivory.BSP.STM32F405.UART
import Ivory.BSP.STM32F405.UART.Tower

import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32.Signalable
import qualified Ivory.BSP.STM32F405.Interrupt as F405 -- XXX required until uartTower is properly polymorphic?

--------------------------------------------------------------------------------

app :: forall p
     . (ColoredLEDs p, PlatformClock p, STM32Signal F405.Interrupt p)
    => Tower p ()
app = do
  stm32f4InitTower
  -- Starts two tasks: a blink task and a controller task.  Periodically blink
  -- the blue LED.
  blinkApp period [blue]
  -- A new queue
  redledctl <- channel
  -- Starts a UART (serial) task
  (istream, ostream) <- uartTower uart5 115200 (Proxy :: Proxy 256)
  -- Start the task defined below
  echoPrompt "hello world" ostream istream (src redledctl)
  -- A task that takes control input (Boolean) from the echo prompt and controls
  -- the red LED based on it.
  task "settableLED" $ ledController [red] (snk redledctl)
  where
  period = 333
  -- red and blue LEDs provided through ColoredLEDs typeclass:
  red  = redLED  (Proxy :: Proxy p)
  blue = blueLED (Proxy :: Proxy p)

--------------------------------------------------------------------------------

echoPrompt :: String
           -> ChannelSource (Stored Uint8)
           -> ChannelSink   (Stored Uint8)
           -> ChannelSource (Stored IBool)
           -> Tower p ()
echoPrompt greeting ostream istream ledctlstream = task "echoprompt" $ do
  o      <- withChannelEmitter ostream      "ostream"
  ledctl <- withChannelEmitter ledctlstream "ledctl"

  let puts :: (GetAlloc eff ~ Scope cs) => String -> Ivory eff ()
      puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str

      putc :: (GetAlloc eff ~ Scope cs) => Uint8 -> Ivory eff ()
      putc = emitV_ o

  taskInit $ do
    puts (greeting ++ "\n")
    puts prompt
  ievt <- withChannelEvent istream "istream"
  handle ievt "istream" $ \inputref -> do
    input <- deref inputref
    putc input -- echo to terminal
    let testChar = (input `isChar`)
    cond_
      [ testChar '1'  ==> emitV_ ledctl true
      , testChar '2'  ==> emitV_ ledctl false
      , testChar '\n' ==> puts prompt
      ]
  where prompt = "tower> "

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

