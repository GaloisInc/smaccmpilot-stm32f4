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


devicedriver :: forall p . (STM32F4Signal p, BoardHSE p)
             => SPIDevice -> Tower p ()
devicedriver device = task "devicedriver" $ do
  taskModuleDef $ hw_moduledef

  taskInit $ do
    gpioSetup     pinE2
    gpioSetup     pinE3
    spiInit       periph
    spiDeviceInit device

  done <- taskLocal "done"

  (txbuffer :: Ref Global (Array 64 (Stored Uint8)))
               <- taskLocal "rxbuffer"
  txbuffersize <- taskLocal "rxbuffersize"
  txbufferpos  <- taskLocal "rxbufferpos"

  (rxbuffer :: Ref Global (Array 64 (Stored Uint8)))
               <- taskLocal "rxbuffer"
  rxbuffersize <- taskLocal "rxbuffersize"
  rxbufferpos  <- taskLocal "rxbufferpos"


  irq <- withUnsafeSignalEvent
                (stm32f4Interrupt interrupt)
                "interrupt"
                (do gpioToggle pinE2
                    modifyReg (spiRegCR2 periph)
                      (clearBit spi_cr2_txeie >>
                       clearBit spi_cr2_rxneie)
                    interrupt_disable interrupt)

  handle irq "irq" $ \_ -> do
    gpioOn pinE3
    tx_pos <- deref txbufferpos
    tx_sz  <- deref txbuffersize
    rx_pos <- deref rxbufferpos
    rx_sz  <- deref rxbuffersize

    sr <- getReg (spiRegSR periph)
    when (bitToBool (sr #. spi_sr_rxne)) $ do
      when (rx_pos <? rx_sz) $ do
        r <- spiGetDR periph
        store (rxbuffer ! rx_pos) r
        store rxbufferpos (rx_pos + 1)
        modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)
      when (rx_pos ==? rx_sz) $ do
        spiDeviceDeselect device
        store done true

    when (bitToBool (sr #. spi_sr_txe)) $ do
      when (tx_pos <? tx_sz) $ do
        w <- deref (txbuffer ! tx_pos)
        store txbufferpos (tx_pos + 1)
        spiSetDR periph w
        modifyReg (spiRegCR2 periph) (setBit spi_cr2_rxneie)

    gpioOff pinE3
    interrupt_enable interrupt

  onPeriod (Milliseconds 100) $ \_ -> do
    ready <- deref done
    when ready $ do
      store done false
      store (txbuffer ! 0) 0xF1
      store (txbuffer ! 1) 0xF2
      store txbufferpos 0
      store txbuffersize 2
      store rxbufferpos 0
      store rxbuffersize 2

      spiDeviceSelect device
      spiBusBegin     platform device
      tx0 <- deref (txbuffer ! 0)
      store txbufferpos 1
      spiSetDR        periph   tx0
      modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)

--      waitTXE
--      spiSetDR        periph   0xF2
--      waitTXE
--      waitNotBSY
--      spiBusEnd       periph
--      spiDeviceDeselect device

  where
  periph = spiDevPeripheral device
  interrupt = spiInterrupt periph

  platform :: Proxy p
  platform = Proxy

  waitTXE :: Ivory (ProcEffects s ()) ()
  waitTXE = do
    comment "wait for txe"
    forever $ do
      sr <- getReg (spiRegSR periph)
      when (bitToBool (sr #. spi_sr_txe))
           breakOut

  waitNotBSY :: Ivory (ProcEffects s ()) ()
  waitNotBSY = do
    comment "wait not busy"
    forever $ do
      sr <- getReg (spiRegSR periph)
      unless (bitToBool (sr #. spi_sr_bsy))
           breakOut



-- Helpers
gpioSetup :: GPIOPin -> Ivory eff ()
gpioSetup p = do
  pinEnable        p
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetSpeed      p gpio_speed_50mhz
  pinSetPUPD       p gpio_pupd_none
  gpioOff          p
gpioOff :: GPIOPin -> Ivory eff ()
gpioOff p = do
  pinClear         p
  pinSetMode       p gpio_mode_output
gpioOn :: GPIOPin -> Ivory eff ()
gpioOn p = do
  pinSet           p
  pinSetMode       p gpio_mode_output
gpioToggle :: GPIOPin -> Ivory eff ()
gpioToggle p = gpioOn p >> gpioOff p
