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
  devicedriver testdevice

debugPin1, debugPin2, debugPin3 :: Maybe GPIOPin
debugPin1 = Just pinE2
debugPin2 = Just pinE4
debugPin3 = Just pinE5

devicedriver :: forall p . (STM32F4Signal p, BoardHSE p)
             => SPIDevice -> Tower p ()
devicedriver device = task "devicedriver" $ do
  taskModuleDef $ hw_moduledef

  done <- taskLocal "done"

  taskInit $ do
    debugSetup     debugPin1
    debugSetup     debugPin2
    debugSetup     debugPin3
    spiInit       periph
    spiDeviceInit device
    store done true


  (txbuffer :: Ref Global (Array 64 (Stored Uint8)))
               <- taskLocal "rxbuffer"
  txbuffersize <- taskLocal "rxbuffersize"
  txbufferpos  <- taskLocal "rxbufferpos"

  (rxbuffer :: Ref Global (Array 64 (Stored Uint8)))
               <- taskLocal "rxbuffer"
  rxbuffersize <- taskLocal "rxbuffersize"
  rxbufferpos  <- taskLocal "rxbufferpos"

  taskPriority 3

  irq <- withUnsafeSignalEvent
                (stm32f4Interrupt interrupt)
                "interrupt"
                (do debugToggle debugPin1
                    modifyReg (spiRegCR2 periph)
                      (clearBit spi_cr2_txeie >>
                       clearBit spi_cr2_rxneie)
                    interrupt_disable interrupt)

  handle irq "irq" $ \_ -> do
    tx_pos <- deref txbufferpos
    tx_sz  <- deref txbuffersize
    rx_pos <- deref rxbufferpos
    rx_sz  <- deref rxbuffersize

    sr <- getReg (spiRegSR periph)
    cond_
      [ bitToBool (sr #. spi_sr_rxne) ==> do
          debugOn debugPin2
          when (rx_pos <? rx_sz) $ do
            r <- spiGetDR periph
            store (rxbuffer ! rx_pos) r
            store rxbufferpos (rx_pos + 1)
            when (rx_pos <=? (rx_sz - 2)) $ do
               modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)
          when (rx_pos ==? (rx_sz - 1)) $ do
            spiBusEnd       periph
            spiDeviceDeselect device
            store done true
          debugOff debugPin2

      , bitToBool (sr #. spi_sr_txe) ==> do
          debugOn debugPin3
          when (tx_pos <? tx_sz) $ do
            w <- deref (txbuffer ! tx_pos)
            spiSetDR periph w
          when (tx_pos <=? tx_sz) $ do
            store txbufferpos (tx_pos + 1)
            modifyReg (spiRegCR2 periph) (setBit spi_cr2_rxneie)
          debugOff debugPin3
      ]

    interrupt_enable interrupt

  onPeriod (Milliseconds 100) $ \_ -> do
    ready <- deref done
    when ready $ do
      store done false
      store (txbuffer ! 0) 0xF1
      store (txbuffer ! 1) 0xF2
      store (txbuffer ! 2) 0xF3
      store txbufferpos 0
      store txbuffersize 3
      store rxbufferpos 0
      store rxbuffersize 3

      spiDeviceSelect device
      spiBusBegin     platform device
      tx0 <- deref (txbuffer ! 0)
      store txbufferpos 1
      spiSetDR        periph   tx0
      modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)
      interrupt_enable interrupt

    unless ready $ do
      return () -- XXX big error - if 100ms have passed
                -- this means the driver has hung somewhere.
                -- Maybe we should try restarting the driver?

  where
  periph = spiDevPeripheral device
  interrupt = spiInterrupt periph

  platform :: Proxy p
  platform = Proxy

-- Helpers
debugSetup :: Maybe GPIOPin -> Ivory eff ()
debugSetup (Just p) = do
  pinEnable        p
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetPUPD       p gpio_pupd_none
  pinClear         p
  pinSetMode       p gpio_mode_output
debugSetup Nothing = return ()

debugOff :: Maybe GPIOPin -> Ivory eff ()
debugOff (Just p) = pinClear p
debugOff Nothing  = return ()

debugOn :: Maybe GPIOPin -> Ivory eff ()
debugOn (Just p) = pinSet p
debugOn Nothing  = return ()

debugToggle :: Maybe GPIOPin -> Ivory eff ()
debugToggle p = debugOn p >> debugOff p

