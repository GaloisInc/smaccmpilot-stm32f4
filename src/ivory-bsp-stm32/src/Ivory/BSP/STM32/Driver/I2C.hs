{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Driver.I2C
  ( i2cTower
  , module Ivory.BSP.STM32.Driver.I2C.Types
  , module Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module


import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Signalable

import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.I2C.Regs
import Ivory.BSP.STM32.Peripheral.I2C.Peripheral
import Ivory.BSP.STM32.PlatformClock

import Ivory.BSP.STM32.Driver.I2C.Types
import Ivory.BSP.STM32.Driver.I2C.I2CDeviceAddr
import Ivory.BSP.STM32.Driver.I2C.I2CDriverState



i2cTower :: (PlatformClock p, STM32Signal p)
         => I2CPeriph (InterruptType p)
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
                     . (STM32Signal p, PlatformClock p)
                    => I2CPeriph (InterruptType p)
                    -> GPIOPin
                    -> GPIOPin
                    -> ChannelSink   (Struct "i2c_transaction_request")
                    -> ChannelSource (Struct "i2c_transaction_result")
                    -> Task p ()
i2cPeripheralDriver periph sda scl req_sink res_source = do
  taskModuleDef $ hw_moduledef

  requestEvent  <- withChannelEvent   req_sink   "req_sink"
  resultEmitter <- withChannelEmitter res_source "res_source"

  state <- taskLocalInit "state" (ival stateInactive)

  taskInit $ do
    debugSetup     debugPin1
    debugSetup     debugPin2
    debugSetup     debugPin3
    debugSetup     debugPin4
    i2cInit        periph sda scl (Proxy :: Proxy p)
    -- Setup hardware for interrupts
    interrupt_set_to_syscall_priority (i2cIntEvent periph)
    interrupt_set_to_syscall_priority (i2cIntError periph)
    interrupt_enable (i2cIntEvent periph)
    interrupt_enable (i2cIntError periph)

  (reqbuffer :: Ref Global (Struct "i2c_transaction_request")) <- taskLocal "reqbuffer"
  (reqbufferpos :: Ref Global (Stored (Ix 128)))               <- taskLocal "reqbufferpos"

  (resbuffer :: Ref Global (Struct "i2c_transaction_result"))  <- taskLocal "resbuffer"
  (resbufferpos :: Ref Global (Stored (Ix 128)))               <- taskLocal "resbufferpos"

  taskPriority 4

  (invalid_request :: Ref Global (Stored Uint32)) <- taskLocal "invalid_request"

  evt_irq <- withUnsafeSignalEvent
                (stm32Interrupt (i2cIntEvent periph))
                "event_interrupt"
                (do debugToggle debugPin1
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_itbufen >> clearBit i2c_cr2_itevten)
                    interrupt_disable (i2cIntEvent periph))

  err_irq <- withUnsafeSignalEvent
                (stm32Interrupt (i2cIntError periph))
                "error_interrupt"
                (do debugToggle debugPin2
                    modifyReg (i2cRegCR2 periph)
                      (clearBit i2c_cr2_iterren)
                    interrupt_disable (i2cIntError periph))

  let sendresult :: Ref s (Struct "i2c_transaction_result") -> Uint8 -> Ivory eff ()
      sendresult res code = do
          debugToggle debugPin4
          store (res ~> resultcode) code
          emit_ resultEmitter (constRef res)

  handle err_irq "error_irq" $ \_ -> do
    -- Must read these registers, sometimes reading dr helps too??
    sr1  <- getReg (i2cRegSR1 periph)
    _sr2 <- getReg (i2cRegSR2 periph)
    _dr  <- getReg (i2cRegDR periph)

    -- If bus error (BERR), acknowledge failure (AF), send Stop
    let must_release = bitToBool (sr1 #. i2c_sr1_berr)
                   .|| bitToBool (sr1 #. i2c_sr1_af)
    when must_release $ setStop periph

    clearSR1 periph

    -- If there is an active transaction, terminate it
    s <- deref state
    cond_
      [ (s ==? stateActive) ==> do
          store state stateInactive
          sendresult resbuffer 1
      , (s ==? stateError) ==> do
          store state stateInactive
      ]

    -- Re-enable interrupt
    modifyReg (i2cRegCR2 periph)
      (setBit i2c_cr2_iterren)
    interrupt_enable (i2cIntError periph)

  handle evt_irq "event_irq" $ \_ -> do
    debugOn debugPin3
    s <- deref state
    cond_
      [ (s ==? stateActive) ==> do
          -- Hardware requires us to read both status registers.
          -- We don't actually need the contents of SR2.
          sr1  <- getReg (i2cRegSR1 periph)
          _sr2 <- getReg (i2cRegSR2 periph)


          btf <- assign (bitToBool (sr1 #. i2c_sr1_btf))

          when (bitToBool (sr1 #. i2c_sr1_sb)) $ do
            tx_sz  <- deref (reqbuffer ~> tx_len)
            tx_pos <- deref reqbufferpos
            tx_ad <- deref (reqbuffer ~> tx_addr)
            let write_remaining = tx_sz - tx_pos
            -- Start bit sent. Send addr field:
            addr <- assign ((write_remaining >? 0) ?
                              (writeAddr tx_ad, readAddr tx_ad))
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


            when btf $ do
              comment "missed received byte??"
              modifyReg (i2cRegCR1 periph) $
                clearBit i2c_cr1_ack
              setStop periph
              store state stateError
              sendresult resbuffer 1

            when (read_remaining ==? 2) $ do
               -- Now 1 remaining
               -- Unset Ack, then Stop
               modifyReg (i2cRegCR1 periph) $
                 clearBit i2c_cr1_ack
               setStop periph

            when ((read_remaining ==? 1) .&& iNot btf) $ do
               -- Now 0 remaining
               store state stateInactive
               sendresult resbuffer 0

          when (bitToBool (sr1 #. i2c_sr1_txe) .&& iNot btf) $ do

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
                  store state stateInactive
                  sendresult resbuffer 0
              ]
      , (s ==? stateError) ==> do
          modifyReg (i2cRegCR1 periph) $ clearBit i2c_cr1_pe
          debugToggle debugPin2 -- XXX PLACEHOLDER
          store state stateInactive
      ]

    debugOff debugPin3
    modifyReg (i2cRegCR2 periph)
      (setBit i2c_cr2_itbufen >> setBit i2c_cr2_itevten)
    interrupt_enable (i2cIntEvent periph)

  handle requestEvent "request" $ \req -> do
    debugOn debugPin3
    s <- deref state
    cond_
      [ (s ==? stateInactive) ==> do
          -- Setup state
          modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_pe
          store state stateActive
          refCopy reqbuffer req
          store reqbufferpos 0
          store resbufferpos 0
          setStart periph
      , true ==> do
          invalid_request %= (+1)
          -- XXX how do we want to handle this error?
          sendresult resbuffer 1
      ]
    debugOff debugPin3

setStop :: I2CPeriph p -> Ivory (ProcEffects eff ()) ()
setStop periph = do
  -- Generate an I2C Stop condition. Per the reference manual, we must
  -- wait for the hardware to clear the stop bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_stop
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_stop)) $
      breakOut

setStart :: I2CPeriph p -> Ivory (ProcEffects eff ()) ()
setStart periph = do
  -- Generate an I2C Start condition. Per the reference manual, we must
  -- wait for the hardware to clear the start bit before any further writes
  -- to CR1. Breaking this rule causes the I2C peripheral to lock up.
  modifyReg (i2cRegCR1 periph) $ setBit i2c_cr1_start
  times (15 :: Ix 16) $ \_ -> do
    cr1 <- getReg (i2cRegCR1 periph)
    unless (bitToBool (cr1 #. i2c_cr1_start)) $
      breakOut

clearSR1 :: I2CPeriph p -> Ivory eff ()
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
debugPin1, debugPin2, debugPin3, debugPin4 :: Maybe GPIOPin
-- Debug 1: toggles on event interrupt
debugPin1 = Nothing -- Just pinB8 -- i2c1 sda
-- Debug 2: toggles on error interrupt
debugPin2 = Nothing -- Just pinB9 -- i2c1 scl
-- Debug 3: active when starting driver with request
debugPin3 = Nothing -- Just pinC12 -- uart5 tx
-- Debug 4: toggles when terminating driver with response
debugPin4 = Nothing -- Just pinD2  -- uart5 rx

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
debugToggle p = do
  -- turn it on three times just to take a bit longer. sometimes my
  -- logic analyzer can't catch a single on/off blip.
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOn p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p
  debugOff p



