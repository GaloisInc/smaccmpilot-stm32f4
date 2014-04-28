{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32F4.I2C.Tower where

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
import Ivory.BSP.STM32F4.I2C.Regs
import Ivory.BSP.STM32F4.I2C.Peripheral
import Ivory.BSP.STM32F4.Interrupt

[ivory|
struct i2c_transaction_request
  { tx_addr   :: Stored Uint8
  ; tx_buf    :: Array 128 (Stored Uint8)
  ; tx_len    :: Stored (Ix 128)
  ; rx_len    :: Stored (Ix 128)
  }
|]

[ivory|
struct i2c_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  }
|]

i2cTower :: (BoardHSE p, STM32F4Signal p)
         => I2CPeriph
         -> GPIOPin
         -> GPIOPin
         -> Tower p ( ChannelSource (Struct "i2c_transaction_request")
                    , ChannelSink   (Struct "i2c_transaction_result"))
i2cTower periph sda scl = do
  towerDepends i2cTowerTypes
  towerModule  i2cTowerTypes
  reqchan <- channel' (Proxy :: Proxy 2) Nothing
  reschan <- channel' (Proxy :: Proxy 2) Nothing
  task ((i2cName periph) ++ "PeripheralDriver") $
    i2cPeripheralDriver periph sda scl (snk reqchan) (src reschan)
  return (src reqchan, snk reschan)

i2cTowerTypes :: Module
i2cTowerTypes = package "i2cTowerTypes" $ do
  defStruct (Proxy :: Proxy "i2c_transaction_request")
  defStruct (Proxy :: Proxy "i2c_transaction_result")

i2cPeripheralDriver :: forall p
                     . (STM32F4Signal p, BoardHSE p)
                    => I2CPeriph
                    -> GPIOPin
                    -> GPIOPin
                    -> ChannelSink   (Struct "i2c_transaction_request")
                    -> ChannelSource (Struct "i2c_transaction_result")
                    -> Task p ()
i2cPeripheralDriver periph sda scl req_sink res_source = do
  taskModuleDef $ hw_moduledef

  requestEvent  <- withChannelEvent   req_sink   "req_sink"
  resultEmitter <- withChannelEmitter res_source "res_source"

  done <- taskLocal "done"

  taskInit $ do
    debugSetup     debugPin1
    debugSetup     debugPin2
    debugSetup     debugPin3
    i2cInit        periph sda scl (Proxy :: Proxy p)
    -- Setup hardware for interrupts
    interrupt_enable (i2cIntEvent periph)
    interrupt_enable (i2cIntError periph)
    store done true

  (reqbuffer :: Ref Global (Struct "i2c_transaction_request")) <- taskLocal "reqbuffer"
  (reqbufferpos :: Ref Global (Stored (Ix 128)))               <- taskLocal "reqbufferpos"

  (resbuffer :: Ref Global (Struct "i2c_transaction_result"))  <- taskLocal "resbuffer"
  (resbufferpos :: Ref Global (Stored (Ix 128)))               <- taskLocal "resbufferpos"

  taskPriority 3

  evt_irq <- withUnsafeSignalEvent
                (stm32f4Interrupt (i2cIntEvent periph))
                "event_interrupt"
                (do debugToggle debugPin1
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_itbufen >> clearBit i2c_cr2_itevten)
                    interrupt_disable (i2cIntEvent periph))

  err_irq <- withUnsafeSignalEvent
                (stm32f4Interrupt (i2cIntError periph))
                "error_interrupt"
                (do debugToggle debugPin2
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_iterren)
                    interrupt_disable (i2cIntError periph))

  handle err_irq "error_irq" $ \_ -> do
    _sr1 <- getReg (i2cRegSR1 periph)
    _sr2 <- getReg (i2cRegSR2 periph)
    -- Clear SR1 - note, reference impl didn't say which bits or why,
    -- so i'm just dupliciating it faithfully, possibly not correctly.
    clearSR1 periph
    -- Send Stop
    setStop periph
    -- If there is an active transaction, terminate it
    active <- iNot `fmap` deref done
    when active $ do
      store done true
      store (resbuffer ~> resultcode) 1
      emit_ resultEmitter (constRef resbuffer)
    modifyReg (i2cRegCR2 periph)
      (setBit i2c_cr2_iterren)
    interrupt_enable (i2cIntError periph)

  handle evt_irq "event_irq" $ \_ -> do
    active <- iNot `fmap` deref done
    when active $ do

      -- Hardware requires us to read both status registers.
      -- We don't actually need the contents of SR2.
      sr1  <- getReg (i2cRegSR1 periph)
      _sr2 <- getReg (i2cRegSR2 periph)

      when (bitToBool (sr1 #. i2c_sr1_sb)) $ do
        tx_sz  <- deref (reqbuffer ~> tx_len)
        tx_pos <- deref reqbufferpos
        tx_ad  <- deref (reqbuffer ~> tx_addr)
        let write_remaining = tx_sz - tx_pos
        -- Start bit sent. Send addr field:
        addr <- assign ((tx_ad * 2) + ((write_remaining >? 0) ? (0,1)))
        modifyReg (i2cRegDR periph) $
          setField i2c_dr_data (fromRep addr)

      when (bitToBool (sr1 #. i2c_sr1_addr)) $ do
        tx_pos <- deref reqbufferpos
        tx_sz  <- deref (reqbuffer ~> tx_len)
        rx_pos <- deref resbufferpos
        rx_sz  <- deref (reqbuffer ~> rx_len)

        let write_remaining = tx_sz - tx_pos
            read_remaining =  rx_sz - rx_pos

        cond_
          [ (write_remaining >? 0) ==> do
              -- Writing: take a byte off the write buffer, write to DR
              w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
              store reqbufferpos (tx_pos + 1)
              modifyReg (i2cRegDR periph) $
                setField i2c_dr_data (fromRep w)
          , (read_remaining >? 1) ==> do
              -- Send an ack on the next byte
              modifyReg (i2cRegCR1 periph) $
                setBit i2c_cr1_ack
          , true ==> do
              -- One byte left to read, send a stop afterwards
              setStop periph
          ]
      when (bitToBool (sr1 #. i2c_sr1_rxne)) $ do
        rx_pos <- deref resbufferpos
        rx_sz  <- deref (reqbuffer ~> rx_len)

        let read_remaining =  rx_sz - rx_pos
        -- Read into the read buffer
        dr <- getReg (i2cRegDR periph)
        r  <- assign (toRep (dr #. i2c_dr_data))
        store ((resbuffer ~> rx_buf) ! rx_pos) r
        store resbufferpos (rx_pos + 1)
        cond_
          [ (read_remaining ==? 2) ==> do -- Now 1 remaining
              -- Unset Ack, then Stop
              modifyReg (i2cRegCR1 periph) $
                clearBit i2c_cr1_ack
              setStop periph
          , (read_remaining ==? 1) ==> do  -- Now 0 remaining
              -- Done - send the i2c_transaction_response
              store done true
              store (resbuffer ~> resultcode) 0
              emit_ resultEmitter (constRef resbuffer)
          ]

      when ((bitToBool (sr1 #. i2c_sr1_txe))
        .&& (iNot (bitToBool (sr1 #. i2c_sr1_btf)))) $ do

        tx_pos <- deref reqbufferpos
        tx_sz  <- deref (reqbuffer ~> tx_len)
        rx_pos <- deref resbufferpos
        rx_sz  <- deref (reqbuffer ~> rx_len)

        let write_remaining = tx_sz - tx_pos
            read_remaining =  rx_sz - rx_pos
        cond_
          -- TXE set, BTF clear: tx buffer is empty, still writing
          [ (write_remaining >? 0) ==> do
              w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
              store reqbufferpos (tx_pos + 1)
              modifyReg (i2cRegDR periph) $
                setField i2c_dr_data (fromRep w)
          , (read_remaining >? 0) ==> do
              -- Done writing, ready to read: send repeated start
              setStart periph
          , true ==> do
              setStop periph
              -- Done, send response
              store done true
              store (resbuffer ~> resultcode) 0
              emit_ resultEmitter (constRef resbuffer)
          ]

    modifyReg (i2cRegCR2 periph)
      (setBit i2c_cr2_itbufen >> setBit i2c_cr2_itevten)
    interrupt_enable (i2cIntEvent periph)

  handle requestEvent "request" $ \req -> do
    d <- deref done
    when d $ do
      debugOn debugPin3
      -- Setup state
      store done false
      refCopy reqbuffer req
      store reqbufferpos 0
      store resbufferpos 0
      setStart periph
      debugOff debugPin3

    unless d $ do
      return () -- XXX how do we want to handle this error?

setStop :: I2CPeriph -> Ivory (ProcEffects eff ()) ()
setStop periph = do
  -- Generate an I2C Stop condition. Per the reference manual, we must
  -- wait for the hardware to clear the stop bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_stop
  forever $ do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_stop)) $
      breakOut

setStart :: I2CPeriph -> Ivory (ProcEffects eff ()) ()
setStart periph = do
  -- Generate an I2C Start condition. Per the reference manual, we must
  -- wait for the hardware to clear the start bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_start
  forever $ do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_start)) $
      breakOut

clearSR1 :: I2CPeriph -> Ivory eff ()
clearSR1 periph = modifyReg (i2cRegSR1 periph) $ do
  clearBit i2c_sr1_smbalert
  clearBit i2c_sr1_timeout
  clearBit i2c_sr1_pecerr
  clearBit i2c_sr1_ovr
  clearBit i2c_sr1_af
  clearBit i2c_sr1_arlo
  clearBit i2c_sr1_berr
  clearBit i2c_sr1_txe
  clearBit i2c_sr1_rxne
  clearBit i2c_sr1_stopf
  clearBit i2c_sr1_add10
  clearBit i2c_sr1_btf
  clearBit i2c_sr1_addr
  clearBit i2c_sr1_sb


-- Debugging Helpers: useful for development, disabled for production.
debugPin1, debugPin2, debugPin3 :: Maybe GPIOPin
--debugPin1 = Nothing
--debugPin2 = Nothing
--debugPin3 = Nothing
debugPin1 = Just pinE2
debugPin2 = Just pinE4
debugPin3 = Just pinE5

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



