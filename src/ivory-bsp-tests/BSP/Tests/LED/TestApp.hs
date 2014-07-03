{-# LANGUAGE ScopedTypeVariables #-}

module BSP.Tests.LED.TestApp where

import Ivory.Language
import Ivory.Tower
import BSP.Tests.Platforms
import BSP.Tests.LED.Blink

app :: forall p . (ColoredLEDs p, BoardInitializer p) => Tower p ()
app = do
  boardInitializer
  blink 250 [redLED platform]
  blink 333 [blueLED platform]
  where
  platform = Proxy :: Proxy p
