{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SPITest where

import Ivory.Language
import Ivory.BitData
import Ivory.HW
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal

import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.Signalable

import Ivory.BSP.STM32F4.SPI.Regs
import Ivory.BSP.STM32F4.SPI.Peripheral
import Ivory.BSP.STM32F4.SPI.Tower

import Platforms

testdevice :: SPIDevice
testdevice = SPIDevice
  { spiDevPeripheral    = spi3
  , spiDevCSPin         = pinE3
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  }

app ::  forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (req, res) <- spiTower [testdevice]
  task "simplecontroller" $ do
    req_emitter <- withChannelEmitter req "req"
    res_event   <- withChannelEvent   res "res"
    periodic    <- withPeriodicEvent (Milliseconds 25)
    handle periodic "periodic" $ \_ -> do
      r <- local $ istruct
             [ tx_device .= ival 0
             , tx_buf    .= iarray [ival 0xF1, ival 0xF2, ival 0xF3]
             , tx_len    .= ival 3
             ]
      emit_ req_emitter (constRef r)
    handle res_event "result" $ \res -> do
      code <- deref (res ~> resultcode)
      buf0 <- deref ((res ~> rx_buf)! 0)
      buf1 <- deref ((res ~> rx_buf)! 1)
      buf2 <- deref ((res ~> rx_buf)! 2)
      idx  <- deref (res ~> rx_idx)
      assert (code ==? 0)
      assert (buf0 ==? 0xFF)
      assert (buf1 ==? 0xFF)
      assert (buf2 ==? 0xFF)
      assert (idx  ==? 3)

