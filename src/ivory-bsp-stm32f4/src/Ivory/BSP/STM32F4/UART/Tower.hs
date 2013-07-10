{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32F4.UART.Tower where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW
import Ivory.HW.Module
import Ivory.BitData

import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART.Regs
import Ivory.BSP.STM32F4.UART.Peripheral

uartTower :: (SingI n, SingI m)
          => UART -> Integer
          -> ChannelSink  n (Stored Uint8)
          -> ChannelSource m (Stored Uint8)
          -> Tower ()
uartTower uart baud ostream istream = do
  let max_syscall_priority = (191::Uint8) -- XXX MAGIC NUMBER: freertos port specific
  -- Manager task:
  -- Initializes the UART and sets up the interrupt
  -- for FreeRTOS.
  -- Event Loop exists because the ostream will not be
  -- read from the ISR unless the TXEIE goes off.
  -- In a typical architectures, you'd set the TXEIE bit
  -- from the task that writes to the ostream.
  task "uartManager" $ do
    o <- withChannelReceiver ostream "ostream"
    taskModuleDef $ const hw_moduledef
    taskBody $ \sch -> do
      uartInit    uart (fromIntegral baud)
      uartInitISR uart max_syscall_priority
      let ohandler = onChannel o $ const $
            setTXEIE uart true
      eventLoop sch ohandler

  -- Signal:
  -- runs the UART Interrupt Service Routine
  signal "uartISR" $ do
    o <- withChannelReceiver ostream "ostream"
    i <- withChannelEmitter  istream "istream"
    signalName (handlerName (uartInterrupt uart))
    signalModuleDef $ const hw_moduledef
    signalBody $ \sch -> do
      sr <- getReg (uartRegSR uart)
      cond_
       [ bitToBool (sr #. uart_sr_rxne) ==> do
           byte <- readDR uart
           bref <- local (ival byte)
           emit_ i (constRef bref)
       , bitToBool (sr #. uart_sr_txe)  ==> do
           byte <- local (ival 0)
           rv   <- sigReceive sch o byte
           ifte_ rv
             (setDR uart =<< deref byte)
             (setTXEIE uart false)
       ]
