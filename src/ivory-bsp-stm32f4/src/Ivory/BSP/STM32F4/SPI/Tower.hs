{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.BSP.STM32F4.SPI.Tower where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW
import Ivory.BitData

import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.SPI

import Ivory.BSP.STM32F4.SPI.Regs
import Ivory.BSP.STM32F4.SPI.Peripheral

[ivory|
struct spi_transmission
  { tx_buf  :: Array 128 (Stored Uint8)
  ; tx_len  :: Stored (Ix 128)
  }
|]

[ivory|
struct spi_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  ; rx_idx     :: Stored (Ix 128)
  }
|]

spiTower :: SPIPeriph
         -> Tower ( ChannelSource 16 (Struct "spi_transmission")
                  , ChannelSink   16 (Struct "spi_transaction_result"))
spiTower spi = do
  addDepends spiTowerTypes
  addModule  spiTowerTypes
  toSig  <- channel
  froSig <- channel
  signal "spiSignal" $ spiSignal spi (snk toSig) (src froSig)
  return (src toSig, snk froSig)

spiTowerTypes :: Module
spiTowerTypes = package "spiTowerTypes" $ do
  defStruct (Proxy :: Proxy "spi_transmission")
  defStruct (Proxy :: Proxy "spi_transaction_result")

spiSignal :: (SingI n, SingI m)
       => SPIPeriph
       -> ChannelSink   n (Struct "spi_transmission")
       -> ChannelSource m (Struct "spi_transaction_result")
       -> Signal ()
spiSignal spi froCtl toCtl = do
  eCtl <- withChannelEmitter  toCtl "toCtl"
  rCtl <- withChannelReceiver froCtl "froCtl"
  signalName $ spiISRHandlerName spi
  signalModuleDef $ const hw_moduledef
  (activestate :: Ref Global (Stored IBool)) <- signalLocal "activestate"
  (txstate     :: Ref Global (Struct "spi_transmission")) <- signalLocal "txstate"
  (txidx       :: Ref Global (Stored (Ix 128))) <- signalLocal "txidx"
  (rxstate     :: Ref Global (Struct "spi_transaction_result")) <- signalLocal "rxstate"
  (rxleft      :: Ref Global (Stored Uint8)) <- signalLocal "rxleft"
  signalModuleDef $ \_sch -> private $ do
    depend spiTowerTypes
  signalBody $ \sch -> do
    active <- deref activestate
    unless active $ do
      got <- sigReceive sch rCtl txstate
      ifte_ got (do
        expected <- deref (txstate ~> tx_len)
        store txidx  0
        store activestate true
        store rxleft (safeCast expected)
        store (rxstate ~> rx_idx) 0
        ) 
        -- The ISR should only go off when inactive if a new
        -- ctl has been posted to rCtl.
        (postError 1 sch eCtl)

    -- then check if there is a new transaction available on
    -- the rCtl channel
    sr <- getReg (spiRegSR spi)
    cond_
      [ bitToBool (sr #. spi_sr_rxne) ==> do
          -- Got rxinterrupt, so 
          dr <- spiGetDR spi
          remaining <- rxStore rxstate rxleft dr
          -- Check if we have received all of the bytes expected
          ifte_ (remaining >? 0)
            -- If there are bytes remaining, enable the tx interrupt
            (spiSetTXEIE spi) $ do
                -- Otherwise, we're done receiving, so disable the
                -- receive interrupt
                spiClearRXNEIE spi
                -- set the isr state to take a new message again next time
                store activestate false
                -- Send eCtl signal indicating transaction complete.
                transactionComplete rxstate sch eCtl
      , bitToBool (sr #. spi_sr_txe) ==> do
          -- Got tx interrupt: disable it
          spiClearTXEIE spi
          -- Check if we have sent all of the bytes expected
          txremain <- txRemaining txstate txidx
          when (txremain >? 0) $ do
            -- Enable rx interrupt, allowing state machine to continue.
            spiSetRXNEIE spi
            -- Get the next byte, and transmit it.
            outgoing <- txPop txstate txidx
            spiSetDR spi outgoing
      ]

  where
  postError ecode sch ch = do
    r <- local (istruct [ resultcode .= (ival ecode)])
    emit_ sch ch (constRef r)
  transactionComplete r sch ch = do
    store (r ~> resultcode) 0 -- Success
    emit_ sch ch (constRef r)

  txRemaining :: Ref s (Struct "spi_transmission") -> Ref s' (Stored (Ix 128))
              -> Ivory eff (Ix 128)
  txRemaining  txstate txidx = do
    len <- deref (txstate ~> tx_len)
    idx <- deref txidx
    return (len - idx)

  txPop :: Ref s (Struct "spi_transmission") -> Ref s' (Stored (Ix 128))
              -> Ivory eff Uint8
  txPop txstate txidx = do
    -- Invariant: txidx <= (txstate ~> tx_len)
    idx <- deref txidx
    v <- deref ((txstate ~> tx_buf) ! idx)
    store txidx (idx + 1)
    return v

  rxStore :: Ref s (Struct "spi_transaction_result") -> Ref s (Stored Uint8)
          -> Uint8 -> Ivory eff Uint8
  rxStore rxstate rxleft v = do
    offs <- deref (rxstate ~> rx_idx)
    store ((rxstate ~> rx_buf) ! offs) v
    store (rxstate ~> rx_idx) (offs + 1)
    left <- deref rxleft
    store rxleft (left - 1)
    return (left - 1)

