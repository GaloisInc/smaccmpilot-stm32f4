{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SMACCMPilot.Datalink.Ivory.UART
  ( framedUARTTower
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART.Regs
import Ivory.BSP.STM32.Peripheral.UART.Peripheral
import Ivory.BSP.STM32.Driver.RingBuffer

framedUARTTower :: (STM32Interrupt s, ANat n)
          => (e -> ClockConfig)
          -> UART s
          -> Integer
          -> Proxy (n :: Nat)
          -> Tower e ( ChanOutput (Stored Uint8) -- Change to cyphertext array
                     , ChanInput  (Stored Uint8)) -- Change to cyphertext array
framedUARTTower tocc uart baud sizeproxy = do

  (src_ostream, snk_ostream) <- channel
  (src_istream, snk_istream) <- channel

  interrupt <- signalUnsafe
    (Interrupt (uartInterrupt uart))
    (Microseconds 50) -- XXX calculate from baud rate.
    (do interrupt_disable (uartInterrupt uart))

  monitor (uartName uart ++ "_driver") $ do
    uartTowerMonitor tocc uart baud sizeproxy interrupt snk_ostream src_istream
  -- Hook up a frame decoder monitor here, to translate the ostream of bytes
  -- into an ostream of cyphertext arrays

  return (snk_istream, src_ostream)

uartTowerMonitor :: forall e n s
                  . (ANat n, STM32Interrupt s)
                 => (e -> ClockConfig)
                 -> UART s
                 -> Integer
                 -> Proxy (n :: Nat)
                 -> ChanOutput (Stored ITime)
                 -> ChanOutput (Stored Uint8)
                 -> ChanInput (Stored Uint8) -- Change to cyphertext array
                 -> Monitor e ()
uartTowerMonitor tocc uart baud _ interrupt ostream istream = do
  clockConfig <- fmap tocc getEnv

  monitorModuleDef $ hw_moduledef

  rxoverruns    <- stateInit (named "rx_overruns") (ival (0 :: Uint32))
  rxsuccess     <- stateInit (named "rx_success") (ival (0 :: Uint32))


  handler systemInit "init" $ callback $ const $ do
    uartInit    uart clockConfig (fromIntegral baud)

-- XXX replace this with a buffer of frames
  (outbuf :: RingBuffer n (Stored Uint8)) <- monitorRingBuffer "outbuf"

  let pop :: Ref s' (Stored Uint8) -> Ivory eff IBool
      pop b = ringbuffer_pop outbuf b

  handler ostream "ostream" $ callback $ \b -> do
    _ <- ringbuffer_push outbuf b -- this remains a straight copy
    setTXEIE uart true

  handler interrupt "interrupt" $ do
    i <- emitter istream 1
    callback $ const $ do
      sr <- getReg (uartRegSR uart)
      when (bitToBool (sr #. uart_sr_orne)) $ do
        byte <- readDR uart
        bref <- local (ival byte)
        emit i (constRef bref)
        rxoverruns %= (+1) -- This is basically an error we can't handle, but its
                           -- useful to be able to check them with gdb
      when (bitToBool (sr #. uart_sr_rxne)) $ do
        byte <- readDR uart
        bref <- local (ival byte)
        emit i (constRef bref)
        rxsuccess %= (+1) -- For debugging
      when (bitToBool (sr #. uart_sr_txe)) $ do
        byte <- local (ival 0)
        rv   <- pop byte -- decode byte
        when rv $ do
          tosend <- deref byte
          setDR uart tosend
      txdone <- ringbuffer_empty outbuf -- Also encoder done
      setTXEIE uart (iNot txdone)
      interrupt_enable (uartInterrupt uart)

  where named n = uartName uart ++ "_" ++ n

