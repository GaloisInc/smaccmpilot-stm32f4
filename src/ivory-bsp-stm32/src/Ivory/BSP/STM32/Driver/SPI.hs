{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.STM32.Driver.SPI
  ( spiTower
  , module Ivory.BSP.STM32.Driver.SPI.Types
  , module Ivory.BSP.STM32.Driver.SPI.SPIDeviceHandle
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module


import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.SPI.Regs
import Ivory.BSP.STM32.Peripheral.SPI.Peripheral

import Ivory.BSP.STM32.Driver.SPI.Types
import Ivory.BSP.STM32.Driver.SPI.SPIDeviceHandle


spiTower :: (PlatformClock p, STM32Signal p)
         => [SPIDevice (InterruptType p)]
         -> Tower p ( ChannelSource (Struct "spi_transaction_request")
                    , ChannelSink   (Struct "spi_transaction_result"))
spiTower devices = do
  towerDepends spiDriverTypes
  towerModule  spiDriverTypes
  reqchan <- channel' (Proxy :: Proxy 2) Nothing
  reschan <- channel' (Proxy :: Proxy 2) Nothing
  task (periphname ++ "PeripheralDriver") $
    spiPeripheralDriver periph devices (snk reqchan) (src reschan)
  return (src reqchan, snk reschan)
  where
  periphname = spiName periph
  periph = case devices of
    [] -> err "for an empty device set"
    d:ds ->
      let canonicalp = spiDevPeripheral d
      in case and (map (\d' -> canonicalp `eqname` spiDevPeripheral d') ds) of
        True -> canonicalp
        False -> err "with devices on different peripherals"
  eqname a b = spiName a == spiName b
  err m = error ("spiTower cannot be created " ++ m)


spiPeripheralDriver :: forall p
                     . (STM32Signal p, PlatformClock p)
                    => SPIPeriph (InterruptType p)
                    -> [SPIDevice (InterruptType p)]
                    -> ChannelSink   (Struct "spi_transaction_request")
                    -> ChannelSource (Struct "spi_transaction_result")
                    -> Task p ()
spiPeripheralDriver periph devices req_sink res_source = do
  taskModuleDef $ hw_moduledef

  requestEvent  <- withChannelEvent   req_sink   "req_sink"
  resultEmitter <- withChannelEmitter res_source "res_source"

  done <- taskLocal "done"

  taskInit $ do
    debugSetup     debugPin1
    debugSetup     debugPin2
    debugSetup     debugPin3
    spiInit        periph
    interrupt_set_to_syscall_priority interrupt
    mapM_ spiDeviceInit devices
    store done true

  reqbuffer    <- taskLocal "reqbuffer"
  reqbufferpos <- taskLocal "reqbufferpos"

  resbuffer    <- taskLocal "resbuffer"
  resbufferpos <- taskLocal "resbufferpos"

  taskPriority 4

  irq <- withUnsafeSignalEvent
                (stm32Interrupt interrupt)
                "interrupt"
                (do debugToggle debugPin1
                    interrupt_disable interrupt)

  handle irq "irq" $ \_ -> do
    tx_pos <- deref reqbufferpos
    tx_sz  <- deref (reqbuffer ~> tx_len)
    rx_pos <- deref resbufferpos
    rx_sz  <- deref (resbuffer ~> rx_idx)

    sr <- getReg (spiRegSR periph)
    cond_
      [ bitToBool (sr #. spi_sr_rxne) ==> do
          debugOn debugPin2
          when (rx_pos <? rx_sz) $ do
            r <- spiGetDR periph
            store ((resbuffer ~> rx_buf) ! rx_pos) r
            store resbufferpos (rx_pos + 1)
          when (rx_pos ==? (rx_sz - 1)) $ do
            modifyReg (spiRegCR2 periph) (clearBit spi_cr2_txeie)
            spiBusEnd       periph
            chooseDevice spiDeviceDeselect (reqbuffer ~> tx_device)
            emit_ resultEmitter (constRef resbuffer)
            store done true
          debugOff debugPin2

      , bitToBool (sr #. spi_sr_txe) ==> do
          debugOn debugPin3
          when (tx_pos <? tx_sz) $ do
            w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
            spiSetDR periph w
          ifte_ (tx_pos <=? tx_sz)
            (do store reqbufferpos (tx_pos + 1)
                modifyReg (spiRegCR2 periph) (setBit spi_cr2_rxneie))
            (modifyReg (spiRegCR2 periph) (clearBit spi_cr2_rxneie))
          debugOff debugPin3
      ]
    interrupt_enable interrupt

  let deviceBeginProc :: SPIDevice i -> Def('[]:->())
      deviceBeginProc dev = proc ((spiDevName dev) ++ "_devicebegin") $
        body $ do
          spiBusBegin platform dev
          spiDeviceSelect dev

  taskModuleDef $ do
    mapM_ (incl . deviceBeginProc) devices

  handle requestEvent "request" $ \req -> do
    ready <- deref done
    when ready $ do
      store done false
      -- Initialize request and result state
      refCopy reqbuffer req
      reqlen <- deref (reqbuffer ~> tx_len)
      store reqbufferpos 0
      store resbufferpos 0
      store (resbuffer ~> rx_idx) reqlen
      -- Get the first byte to transmit
      tx0 <- deref ((reqbuffer ~> tx_buf) ! 0)
      store reqbufferpos 1
      -- select the device and setup the spi peripheral
      chooseDevice (call_ . deviceBeginProc) (reqbuffer ~> tx_device)
      -- Send the first byte, enable tx empty interrupt
      spiSetDR  periph tx0
      modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)
      interrupt_enable interrupt

    unless ready $ do
      return () -- XXX how do we want to handle this error?

  where
  interrupt = spiInterrupt periph
  platform :: Proxy p
  platform = Proxy


  chooseDevice :: (SPIDevice (InterruptType p) -> Ivory eff ())
               -> Ref Global (Stored SPIDeviceHandle) -> Ivory eff ()
  chooseDevice callback devref = do
    comment "selecting device:"
    currdev <- deref devref
    assert (currdev <? invalidhandle)
    cond_ (zipWith (aux currdev) devices [(0::Integer)..])
    comment "end selecting configured device"
    where
    invalidhandle = SPIDeviceHandle (fromIntegral (length devices))
    aux cd device idx =
      cd ==? SPIDeviceHandle (fromIntegral idx) ==> callback device


-- Debugging Helpers: useful for development, disabled for production.
debugPin1, debugPin2, debugPin3 :: Maybe GPIOPin
debugPin1 = Nothing
debugPin2 = Nothing
debugPin3 = Nothing
--debugPin1 = Just pinE2
--debugPin2 = Just pinE4
--debugPin3 = Just pinE5

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

