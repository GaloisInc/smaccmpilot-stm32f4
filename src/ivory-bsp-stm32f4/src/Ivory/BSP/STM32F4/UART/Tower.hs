{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

uartTower :: forall n p
           . (SingI n, BoardHSE p, STM32F4Signal p)
          => UART
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower p ( ChannelSink   (Stored Uint8)
                     , ChannelSource (Stored Uint8))
uartTower uart baud sizeproxy = do
  -- MAGIC NUMBER: freertos syscalls must be lower (numerically greater
  -- than) level 11
  let max_syscall_priority = (12::Uint8)

  (src_ostream, snk_ostream) <- channelWithSize sizeproxy
  (src_istream, snk_istream) <- channelWithSize sizeproxy

  task (uartName uart ++ "_driver") $ do

    o <- withChannelReceiver snk_ostream "ostream"
    i <- withChannelEmitter  src_istream "istream"

    taskModuleDef $ hw_moduledef

    txpending     <- taskLocal "txpending"
    txpendingbyte <- taskLocal "txpendingbyte"

    txcheck <- timerEvent txcheck_period
    interrupt <- withUnsafeSignalEvent
      (stm32f4Interrupt (uartInterrupt uart))
      (uartName uart ++ "_isr")
      (interrupt_disable (uartInterrupt uart))

    taskInit $ do
      store txpending false
      uartInit    uart (Proxy :: Proxy p) (fromIntegral baud)
      uartInitISR uart max_syscall_priority

    handle interrupt "interrupt" $ \_msg -> do
      sr <- getReg (uartRegSR uart)
      cond_
       [ bitToBool (sr #. uart_sr_rxne) ==> do
           byte <- readDR uart
           bref <- local (ival byte)
           emit_ i (constRef bref)
       , bitToBool (sr #. uart_sr_txe) ==> do
           pending <- deref txpending
           ifte_ pending
             (do store txpending false
                 setDR uart =<< deref txpendingbyte)
             (do byte <- local (ival 0)
                 rv   <- receive o byte
                 ifte_ rv
                   (setDR uart =<< deref byte)
                   (setTXEIE uart false))
       ]
      interrupt_enable (uartInterrupt uart)

    handle txcheck "txcheck" $ \_ -> do
      txeie <- getTXEIE uart
      pending <- deref txpending
      unless (txeie .&& iNot pending) $ do
        byte <- local (ival 0)
        txready <- receive o byte
        when txready $ do
          store txpending true
          store txpendingbyte =<< deref byte
          setTXEIE uart true



  return (snk_istream, src_ostream)

  where
  txcheck_period = Milliseconds 1

