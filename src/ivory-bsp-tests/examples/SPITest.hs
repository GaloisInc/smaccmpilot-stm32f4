{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SPITest where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable

import Ivory.BSP.STM32F4.SPI

import Platforms

testdevice1 :: SPIDevice
testdevice1 = SPIDevice
  { spiDevPeripheral    = spi3
  , spiDevCSPin         = pinE2
  , spiDevClockHz       = 2500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  , spiDevName          = "testdevice1_2500khz_pinE2"
  }

testdevice2 :: SPIDevice
testdevice2 = SPIDevice
  { spiDevPeripheral    = spi3
  , spiDevCSPin         = pinE3
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  , spiDevName          = "testdevice2_500khz_pinE3"
  }

app ::  forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (req, res) <- spiTower [testdevice1, testdevice2]

  task "simplecontroller" $ do
    req_emitter <- withChannelEmitter req "req"
    res_event   <- withChannelEvent   res "res"
    periodic    <- withPeriodicEvent (Milliseconds 250)
    handleV periodic "periodic" $ \p -> do
      ifte_ ((p .% 500000) >=? 250000)
        (do r <- local $ istruct
                   [ tx_device .= ival (SPIDeviceHandle 0) -- Should be pinE2 / 1mhz
                   , tx_buf    .= iarray [ival 0xF1, ival 0xF2, ival 0xF3]
                   , tx_len    .= ival 3
                   ]
            emit_ req_emitter (constRef r))
        (do r <- local $ istruct
                   [ tx_device .= ival (SPIDeviceHandle 1) -- Should be pinE3 / 500khz
                   , tx_buf    .= iarray [ival 0xF4, ival 0xF5, ival 0xF6, ival 0xF7]
                   , tx_len    .= ival 4
                   ]
            emit_ req_emitter (constRef r))

    handle res_event "result" $ \r -> do
      code <- deref (r ~> resultcode)
      len  <- deref (r ~> rx_idx)
      assert (code ==? 0)
      assert ((len  ==? 3) .|| (len ==? 4))

