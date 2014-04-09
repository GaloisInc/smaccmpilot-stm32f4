{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32F4.UART.Tower where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module
import Ivory.BitData

import Ivory.BSP.STM32F4.Signalable
import Ivory.BSP.STM32F4.UART.Regs
import Ivory.BSP.STM32F4.UART.Peripheral
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.Interrupt

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

uartTower :: forall n p
           . (SingI n, BoardHSE p, STM32F4Signal p)
          => UART
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8))
uartTower u b s = uartTowerDebuggable u b s dbg
  where dbg = UARTTowerDebugger
          { debug_init = return ()
          , debug_isr  = return ()
          , debug_evthandler_start = return ()
          , debug_evthandler_end = return ()
          , debug_txcheck = return ()
          , debug_txcheck_pend = return ()
          , debug_txeie = const (return ())
          }

uartTowerDebuggable :: forall n p
           . (SingI n, BoardHSE p, STM32F4Signal p)
          => UART
          -> Integer
          -> Proxy (n :: Nat)
          -> UARTTowerDebugger
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8))
uartTowerDebuggable uart baud sizeproxy dbg = do
  -- MAGIC NUMBER: freertos syscalls must be lower (numerically greater
  -- than) level 11
  let max_syscall_priority = (12::Uint8)

  (src_ostream, snk_ostream) <- channel' sizeproxy Nothing
  (src_istream, snk_istream) <- channel' sizeproxy Nothing

  task (uartName uart ++ "_driver") $ do
    o <- withChannelReceiver snk_ostream "ostream"
    i <- withChannelEmitter  src_istream "istream"

    taskPriority 4 -- XXX Kinda arbitrary...
    taskModuleDef $ hw_moduledef

    rxoverruns    <- taskLocalInit "rxoverruns" (ival (0 :: Uint32))
    rxsuccess     <- taskLocalInit "rxsuccess" (ival (0 :: Uint32))
    txpending     <- taskLocal "txpending"
    txpendingbyte <- taskLocal "txpendingbyte"

    txcheck <- timerEvent txcheck_period
    interrupt <- withUnsafeSignalEvent
      (stm32f4Interrupt (uartInterrupt uart))
      (uartName uart ++ "_isr")
      (do debug_isr dbg
          setTXEIE uart false
          setRXNEIE uart false
          interrupt_disable (uartInterrupt uart))
      --    interrupt_clear_pending (uartInterrupt uart))

    taskInit $ do
      debug_init dbg
      store txpending false
      uartInit    uart (Proxy :: Proxy p) (fromIntegral baud)
      uartInitISR uart max_syscall_priority

    handle interrupt "interrupt" $ \_msg -> do
      debug_evthandler_start dbg
      continueTXEIE <- local (ival false)
      sr <- getReg (uartRegSR uart)
      when (bitToBool (sr #. uart_sr_orne)) $ do
        byte <- readDR uart
        bref <- local (ival byte)
        emit_ i (constRef bref)
        rxoverruns %= (+1) -- This is basically an error we can't handle...
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

    handle txcheck "txcheck" $ \_ -> do
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



  return (snk_istream, src_ostream)

  where
  txcheck_period = Milliseconds 1

