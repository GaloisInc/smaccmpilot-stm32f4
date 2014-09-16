{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.STM32.Driver.CAN
  ( canTower
  , module Ivory.BSP.STM32.Driver.CAN.Types
  ) where

import Control.Monad (forM_)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Signal (withUnsafeSignalEvent)
import Ivory.HW
import Ivory.HW.Module


import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4

import Ivory.BSP.STM32.Driver.CAN.Types


canTower :: (PlatformClock p, STM32Signal p)
         => CANPeriph (InterruptType p)
         -> Integer
         -> GPIOPin
         -> GPIOPin
         -> Tower p ( ChannelSource (Struct "can_transmit_request")
                    , ChannelSink   (Struct "can_receive_result"))
canTower periph bitrate rxpin txpin = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes
  pendingRequests <- channel' (Proxy :: Proxy 2) Nothing
  reqchan <- channel' (Proxy :: Proxy 2) Nothing
  reschan <- channel' (Proxy :: Proxy 16) Nothing
  task (canName periph ++ "PeripheralDriver") $
    canPeripheralDriver periph bitrate rxpin txpin (snk reqchan) (src reschan) pendingRequests
  return (src reqchan, snk reschan)


canPeripheralDriver :: forall p
                     . (STM32Signal p, PlatformClock p)
                    => CANPeriph (InterruptType p)
                    -> Integer
                    -> GPIOPin
                    -> GPIOPin
                    -> ChannelSink   (Struct "can_transmit_request")
                    -> ChannelSource (Struct "can_receive_result")
                    -> (ChannelSource (Struct "can_transmit_request"), ChannelSink (Struct "can_transmit_request"))
                    -> Task p ()
canPeripheralDriver periph bitrate rxpin txpin req_sink res_source pendingRequests = do
  taskModuleDef $ hw_moduledef

  nextRequest <- withChannelReceiver (snk pendingRequests) "pend_sink"
  pendRequest <- withChannelEmitter (src pendingRequests) "pend_source"

  requestEvent  <- withChannelEvent req_sink "req_sink"
  resultEmitter <- withChannelEmitter res_source "res_source"

  taskInit $ do
    canInit periph bitrate rxpin txpin (Proxy :: Proxy p)
    modifyReg (canRegIER periph) $ do
      setBit can_ier_tmeie
      setBit can_ier_fmpie0
      setBit can_ier_fmpie1
    interrupt_set_to_syscall_priority $ canIntTX periph
    interrupt_set_to_syscall_priority $ canIntSCE periph
    interrupt_enable $ canIntTX periph
    forM_ (canRegRX periph) $ \ fifo -> do
      interrupt_set_to_syscall_priority $ canIntRX fifo
      interrupt_enable $ canIntRX fifo

  taskPriority 4

  let emitMessage = proc "emitMessage" $ \ rir rdtr rdlr rdhr -> body $ do
        ide <- assign $ bitToBool $ rir #. can_rir_ide
        stid <- assign $ safeCast $ toRep $ rir #. can_rir_stid
        ident <- assign $ ide ? (stid `iShiftL` 18 .| (toRep $ rir #. can_rir_exid), stid)

        msg <- local $ istruct
          [ rx_id .= ival ident
          , rx_ide .= ival ide
          , rx_rtr .= ival (bitToBool $ rir #. can_rir_rtr)
          , rx_buf .= iarray (map ival $ map toRep $
              map (rdlr #.) [can_rdlr_data0, can_rdlr_data1, can_rdlr_data2, can_rdlr_data3] ++
              map (rdhr #.) [can_rdhr_data4, can_rdhr_data5, can_rdhr_data6, can_rdhr_data7])
          , rx_len .= ival (toIx $ toRep $ rdtr #. can_rdtr_dlc)
          , rx_fmi .= ival (toRep $ rdtr #. can_rdtr_fmi)
          , rx_time .= ival (toRep $ rdtr #. can_rdtr_time)
          ]
        emit_ resultEmitter $ constRef msg
        retVoid

  let receiveMessage fifo = do
      forever $ do
        rfr <- getReg $ canRegRFR fifo
        -- If the FIFO is empty, we have nothing to do.
        when (rfr #. can_rfr_fmp ==? fromRep 0) breakOut

        -- If the FIFO is still reloading the mailbox, we can't pull the
        -- next message out. All we can do is loop and try again.
        unless (bitToBool $ rfr #. can_rfr_rfom) $ do
          rir <- getReg $ canRegRIR fifo
          rdtr <- getReg $ canRegRDTR fifo
          rdlr <- getReg $ canRegRDLR fifo
          rdhr <- getReg $ canRegRDHR fifo

          -- Release this FIFO entry ASAP to overlap the mailbox reload
          -- with the emitMessage call.
          setReg (canRegRFR fifo) $ do
            setBit can_rfr_rfom
            setBit can_rfr_fovr
            setBit can_rfr_full

          call_ emitMessage rir rdtr rdlr rdhr

      interrupt_enable $ canIntRX fifo

  forM_ (zip [0 :: Int ..] $ canRegRX periph) $ \ (idx, fifo) -> do
    rx_irq <- withUnsafeSignalEvent
                  (stm32Interrupt $ canIntRX fifo)
                  ("rx" ++ show idx ++ "_interrupt")
                  (interrupt_disable $ canIntRX fifo)
    handle rx_irq ("rx" ++ show idx ++ "_irq") $ \_ -> receiveMessage fifo

  let sendRequest :: Def ('[ConstRef s1 (Struct "can_transmit_request")] :-> ())
      sendRequest = proc "sendRequest" $ \ req -> body $ do
      can_id <- deref (req ~> tx_id)
      ide <- deref (req ~> tx_ide)
      let stid = lbits $ ide ? (can_id `iShiftR` 18, can_id)
      let exid = ide ? (can_id, 0)
      rtr <- deref (req ~> tx_rtr)
      len <- deref (req ~> tx_len)

      let get_bytes :: (BitData reg, SafeCast Uint8 (BitDataRep reg)) => [(Ix 8, BitDataField reg (Bits 8))] -> Ivory eff [BitDataM reg ()]
          get_bytes = mapM $ \ (idx, field) -> do
            v <- deref $ (req ~> tx_buf) ! idx
            return $ setField field $ fromRep v

      low_bytes <- get_bytes [(0, can_tdlr_data0), (1, can_tdlr_data1), (2, can_tdlr_data2), (3, can_tdlr_data3)]
      hi_bytes <- get_bytes [(4, can_tdhr_data4), (5, can_tdhr_data5), (6, can_tdhr_data6), (7, can_tdhr_data7)]

      tsr <- getReg (canRegTSR periph)
      mailbox_code <- assign $ tsr #. can_tsr_code
      assert $ toRep mailbox_code <? fromIntegral (length $ canRegTX periph)

      enqueued <- local izero
      cond_
        [ mailbox_code ==? fromRep (fromInteger mailbox_idx) ==> do
          store enqueued $ bitToBool $ tsr #. canTXEmpty txmailbox
          enqueued' <- deref enqueued
          when enqueued' $ do
            modifyReg (canRegTDTR txmailbox) $ do
              clearBit can_tdtr_tgt
              setField can_tdtr_dlc $ fromRep $ castDefault $ fromIx len
            setReg (canRegTDLR txmailbox) $ sequence_ low_bytes
            setReg (canRegTDHR txmailbox) $ sequence_ hi_bytes
            setReg (canRegTIR txmailbox) $ do
              setField can_tir_stid $ fromRep stid
              setField can_tir_exid $ fromRep exid
              setField can_tir_ide $ boolToBit ide
              setField can_tir_rtr $ boolToBit rtr
              setBit can_tir_txrq
        | (mailbox_idx, txmailbox) <- zip [0..] $ canRegTX periph
        ]
      enqueued' <- deref enqueued
      unless enqueued' $ emit_ pendRequest req

  tx_irq <- withUnsafeSignalEvent
                (stm32Interrupt $ canIntTX periph)
                "tx_interrupt"
                (interrupt_disable $ canIntTX periph)

  handle tx_irq "tx_irq" $ \_ -> do
    tsr <- getReg $ canRegTSR periph
    next <- local izero
    -- Refill all mailboxes that have finished transmitting.
    forM_ [can_tsr_rqcp0, can_tsr_rqcp1, can_tsr_rqcp2] $ \ rqcp_field ->
      when (bitToBool $ tsr #. rqcp_field) $ do
        -- Acknowledge completion by writing a 1 to the completion
        -- field. Note that the other fields are either read-only or
        -- only do something if you write a 1 to them. So we don't need
        -- a read-modify-write cycle, and in fact using modifyReg could
        -- be harmful.
        setReg (canRegTSR periph) $ setBit rqcp_field
        sendMore <- receive nextRequest next
        when sendMore $ call_ sendRequest $ constRef next
    interrupt_enable $ canIntTX periph

  handle requestEvent "request" $ call_ sendRequest

  taskModuleDef $ do
    incl sendRequest
    incl emitMessage
