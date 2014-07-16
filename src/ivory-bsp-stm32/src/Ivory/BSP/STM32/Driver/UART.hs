{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32.Driver.UART
  ( uartTower
  , uartTowerFlushable
  , uartTowerDebuggable
  , UARTTowerDebugger(..)
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

import Ivory.BSP.STM32.Peripheral.UART.Regs
import Ivory.BSP.STM32.Peripheral.UART.Peripheral

data UARTTowerDebugger =
  UARTTowerDebugger
    { debug_init             :: forall eff . Ivory eff ()
    , debug_isr              :: forall eff . Ivory eff ()
    , debug_evthandler_start :: forall eff . Ivory eff ()
    , debug_evthandler_end   :: forall eff . Ivory eff ()
    , debug_txcheck          :: forall eff . Ivory eff ()
    , debug_txcheck_pend     :: forall eff . Ivory eff ()
    , debug_txeie            :: forall eff . IBool -> Ivory eff ()
    }

emptyDbg :: UARTTowerDebugger
emptyDbg =
  UARTTowerDebugger
    { debug_init = return ()
    , debug_isr  = return ()
    , debug_evthandler_start = return ()
    , debug_evthandler_end = return ()
    , debug_txcheck = return ()
    , debug_txcheck_pend = return ()
    , debug_txeie = const (return ())
    }

uartTower :: forall n p
           . (ANat n, PlatformClock p, STM32Signal p)
          => UART (InterruptType p)
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8))
uartTower u b s = uartTowerDebuggable u b s emptyDbg

uartTowerFlushable :: forall n p
           . (ANat n, PlatformClock p, STM32Signal p)
          => UART (InterruptType p)
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8)
                     , ChannelSource (Stored ITime))
uartTowerFlushable uart baud sizeproxy = do
  (src_ostream, snk_ostream) <- channel' sizeproxy Nothing
  (src_istream, snk_istream) <- channel' sizeproxy Nothing
  (src_flush, snk_flush) <- channel' (Proxy :: Proxy 2) Nothing

  task (uartName uart ++ "_flushable_driver") $ do
    txcheck_evt <- withChannelEvent snk_flush "flush"
    uartTowerTask uart baud snk_ostream src_istream txcheck_evt emptyDbg

  return (snk_istream, src_ostream, src_flush)


uartTowerDebuggable :: forall n p
           . (ANat n, PlatformClock p, STM32Signal p)
          => UART (InterruptType p)
          -> Integer
          -> Proxy (n :: Nat)
          -> UARTTowerDebugger
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8))
uartTowerDebuggable uart baud sizeproxy dbg = do

  (src_ostream, snk_ostream) <- channel' sizeproxy Nothing
  (src_istream, snk_istream) <- channel' sizeproxy Nothing

  task (uartName uart ++ "_driver") $ do
    txcheck_evt <- withPeriodicEvent txcheck_period
    uartTowerTask uart baud snk_ostream src_istream txcheck_evt dbg

  return (snk_istream, src_ostream)

  where
  txcheck_period = Milliseconds 1


uartTowerTask :: forall p
               . (STM32Signal p, PlatformClock p)
              => UART (InterruptType p)
              -> Integer
              -> ChannelSink (Stored Uint8)
              -> ChannelSource (Stored Uint8)
              -> Event (Stored ITime)
              -> UARTTowerDebugger
              -> Task p ()
uartTowerTask uart baud snk_ostream src_istream txcheck_evt dbg = do
  o <- withChannelReceiver snk_ostream "ostream"
  i <- withChannelEmitter  src_istream "istream"

  taskPriority 4 -- XXX Kinda arbitrary...
  taskModuleDef $ hw_moduledef

  rxoverruns    <- taskLocalInit (named "rx_overruns") (ival (0 :: Uint32))
  rxsuccess     <- taskLocalInit (named "rx_success") (ival (0 :: Uint32))
  txpending     <- taskLocal (named "tx_pending")
  txpendingbyte <- taskLocal (named "tx_pendingbyte")

  interrupt <- withUnsafeSignalEvent
    (stm32Interrupt (uartInterrupt uart))
    (uartName uart ++ "_isr")
    (do debug_isr dbg
        setTXEIE uart false
        setRXNEIE uart false
        interrupt_disable (uartInterrupt uart))

  taskInit $ do
    debug_init dbg
    store txpending false
    uartInit    uart (Proxy :: Proxy p) (fromIntegral baud)
    uartInitISR uart

  handle interrupt "interrupt" $ \_msg -> do
    debug_evthandler_start dbg
    continueTXEIE <- local (ival false)
    sr <- getReg (uartRegSR uart)
    when (bitToBool (sr #. uart_sr_orne)) $ do
      byte <- readDR uart
      bref <- local (ival byte)
      emit_ i (constRef bref)
      rxoverruns %= (+1) -- This is basically an error we can't handle, but its
                         -- useful to be able to check them with gdb
    when (bitToBool (sr #. uart_sr_rxne)) $ do
      byte <- readDR uart
      bref <- local (ival byte)
      emit_ i (constRef bref)
      rxsuccess %= (+1) -- For debugging
    when (bitToBool (sr #. uart_sr_txe)) $ do
      pending <- deref txpending
      ifte_ pending
        (do store txpending false
            tosend <- deref txpendingbyte
            store continueTXEIE true
            setDR uart tosend)
        (do byte <- local (ival 0)
            rv   <- receive o byte
            when rv $ do
              tosend <- deref byte
              store continueTXEIE true
              setDR uart tosend)
    debug_evthandler_end dbg
    setTXEIE uart =<< deref continueTXEIE
    setRXNEIE uart true
    interrupt_enable (uartInterrupt uart)

  handle txcheck_evt "txcheck" $ \_ -> do
    txeie <- getTXEIE uart
    pending <- deref txpending
    unless (txeie .&& iNot pending) $ do
      debug_txcheck dbg
      byte <- local (ival 0)
      txready <- receive o byte
      when txready $ do
        debug_txcheck_pend dbg
        store txpending true
        store txpendingbyte =<< deref byte
        setTXEIE uart true


  where named n = (uartName uart) ++ "_"++ n
