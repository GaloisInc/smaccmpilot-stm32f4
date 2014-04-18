{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32F4.SPI.Tower where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module
import Ivory.BitData

import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Signalable
import Ivory.BSP.STM32F4.SPI.Regs
import Ivory.BSP.STM32F4.SPI.Peripheral
import Ivory.BSP.STM32F4.Interrupt

[ivory|
struct spi_transaction_request
  { tx_device :: Stored Uint8
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  }
|]

[ivory|
struct spi_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  ; rx_idx     :: Stored (Ix 128)
  }
|]

spiTower :: (BoardHSE p, STM32F4Signal p)
         => [SPIDevice]
         -> Tower p ( ChannelSource (Struct "spi_transaction_request")
                    , ChannelSink   (Struct "spi_transaction_result"))
spiTower devices = do
  towerDepends spiTowerTypes
  towerModule  spiTowerTypes
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

spiTowerTypes :: Module
spiTowerTypes = package "spiTowerTypes" $ do
  defStruct (Proxy :: Proxy "spi_transaction_request")
  defStruct (Proxy :: Proxy "spi_transaction_result")

spiPeripheralDriver :: forall p
                     . (STM32F4Signal p, BoardHSE p)
                    => SPIPeriph
                    -> [SPIDevice]
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
    mapM_ spiDeviceInit devices
    store done true

  reqbuffer    <- taskLocal "reqbuffer"
  reqbufferpos <- taskLocal "reqbufferpos"

  resbuffer    <- taskLocal "resbuffer"
  resbufferpos <- taskLocal "resbufferpos"

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
            when (rx_pos <=? (rx_sz - 2)) $ do
               modifyReg (spiRegCR2 periph) (setBit spi_cr2_txeie)
          when (rx_pos ==? (rx_sz - 1)) $ do
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
          when (tx_pos <=? tx_sz) $ do
            store reqbufferpos (tx_pos + 1)
            modifyReg (spiRegCR2 periph) (setBit spi_cr2_rxneie)
          debugOff debugPin3
      ]

    interrupt_enable interrupt

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
      chooseDevice spiDeviceSelect        (reqbuffer ~> tx_device)
      chooseDevice (spiBusBegin platform) (reqbuffer ~> tx_device)
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


  chooseDevice :: (SPIDevice -> Ivory eff ())
               -> Ref Global (Stored Uint8) -> Ivory eff ()
  chooseDevice callback devref = do
    comment "selecting device:"
    currdev <- deref devref
    assert (currdev <? (fromIntegral (length devices)))
    cond_ (zipWith (aux currdev) devices [(0::Integer)..])
    comment "end selecting configured device"
    where
    aux cd device idx = cd ==? (fromIntegral idx) ==> callback device


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

