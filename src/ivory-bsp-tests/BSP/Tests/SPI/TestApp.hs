{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BSP.Tests.SPI.TestApp where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.PlatformClock
import Ivory.BSP.STM32F405.GPIO

import BSP.Tests.Platforms

app ::  forall p . (ColoredLEDs p, PlatformClock p, BoardInitializer p, TestSPI p)
    => Tower p ()
app = do
  boardInitializer
  (req, res) <- spiTower [ testdevice1, testdevice2 ]

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

  where
  periph = testSPI (Proxy :: Proxy p)
  testdevice1 = SPIDevice
    { spiDevPeripheral    = periph
    , spiDevCSPin         = pinE2
    , spiDevClockHz       = 2500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "testdevice1_2500khz_pinE2"
    }
  testdevice2 = SPIDevice
    { spiDevPeripheral    = periph
    , spiDevCSPin         = pinE3
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "testdevice2_500khz_pinE3"
    }
