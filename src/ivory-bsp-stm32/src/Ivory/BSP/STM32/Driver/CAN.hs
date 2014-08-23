{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.STM32.Driver.CAN
  ( canTower
  , module Ivory.BSP.STM32.Driver.CAN.Types
  ) where

import Control.Monad (zipWithM_, forM_)
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

import Ivory.BSP.STM32.Driver.CAN.Types


canTower :: (PlatformClock p, STM32Signal p)
         => CANPeriph (InterruptType p)
         -> Integer
         -> Tower p ( ChannelSource (Struct "can_transmit_request")
                    , ChannelSink   (Struct "can_receive_result"))
canTower periph bitrate = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes
  pendingRequests <- channel' (Proxy :: Proxy 2) Nothing
  reqchan <- channel' (Proxy :: Proxy 2) Nothing
  reschan <- channel' (Proxy :: Proxy 16) Nothing
  task (canName periph ++ "PeripheralDriver") $
    canPeripheralDriver periph bitrate (snk reqchan) (src reschan) pendingRequests
  return (src reqchan, snk reschan)


canPeripheralDriver :: forall p
                     . (STM32Signal p, PlatformClock p)
                    => CANPeriph (InterruptType p)
                    -> Integer
                    -> ChannelSink   (Struct "can_transmit_request")
                    -> ChannelSource (Struct "can_receive_result")
                    -> (ChannelSource (Struct "can_transmit_request"), ChannelSink (Struct "can_transmit_request"))
                    -> Task p ()
canPeripheralDriver periph bitrate req_sink _res_source pendingRequests = do
  taskModuleDef $ hw_moduledef

  nextRequest <- withChannelReceiver (snk pendingRequests) "pend_sink"
  pendRequest <- withChannelEmitter (src pendingRequests) "pend_source"

  requestEvent  <- withChannelEvent req_sink "req_sink"
  -- resultEmitter <- withChannelEmitter res_source "res_source"

  taskInit $ do
    canInit periph bitrate (Proxy :: Proxy p)
    modifyReg (canRegIER periph) $ setBit can_ier_tmeie
    interrupt_set_to_syscall_priority $ canIntTX periph
    interrupt_set_to_syscall_priority $ canIntRX0 periph
    interrupt_set_to_syscall_priority $ canIntRX1 periph
    interrupt_set_to_syscall_priority $ canIntSCE periph
    interrupt_enable $ canIntTX periph

  taskPriority 4

  let sendRequest :: Def ('[ConstRef s1 (Struct "can_transmit_request")] :-> ())
      sendRequest = proc "sendRequest" $ \ req -> body $ do
      can_id <- deref (req ~> tx_id)
      ide <- deref (req ~> tx_ide)
      let stid = lbits $ ide ? (can_id `iShiftR` 17, can_id)
      let exid = ide ? (can_id, 0)
      rtr <- deref (req ~> tx_rtr)
      len <- deref (req ~> tx_len)
      low_bytes <- mapM (\idx-> fmap fromRep $ deref ((req ~> tx_buf) ! idx)) [0, 1, 2, 3]
      hi_bytes <- mapM (\idx-> fmap fromRep $ deref ((req ~> tx_buf) ! idx)) [4, 5, 6, 7]

      tsr <- getReg (canRegTSR periph)
      mailbox_code <- assign $ tsr #. can_tsr_code
      assert (toRep mailbox_code <? 3)

      enqueued <- local izero
      cond_
        [ mailbox_code ==? fromRep (fromIntegral mailbox_idx) ==> do
          store enqueued $ bitToBool $ tsr #. ([can_tsr_tme0, can_tsr_tme1, can_tsr_tme2] !! mailbox_idx)
          enqueued' <- deref enqueued
          when enqueued' $ do
            let txmailbox = canRegTX periph !! mailbox_idx
            modifyReg (canRegTDTR txmailbox) $ do
              clearBit can_tdtr_tgt
              setField can_tdtr_dlc $ fromRep $ castDefault $ fromIx len
            setReg (canRegTDLR txmailbox) $
              zipWithM_ setField [can_tdlr_data0, can_tdlr_data1, can_tdlr_data2, can_tdlr_data3] low_bytes
            setReg (canRegTDHR txmailbox) $
              zipWithM_ setField [can_tdhr_data4, can_tdhr_data5, can_tdhr_data6, can_tdhr_data7] hi_bytes
            setReg (canRegTIR txmailbox) $ do
              setField can_tir_stid $ fromRep stid
              setField can_tir_exid $ fromRep exid
              setField can_tir_ide $ boolToBit ide
              setField can_tir_rtr $ boolToBit rtr
              setBit can_tir_txrq
        | mailbox_idx <- [0..2]
        ]
      enqueued' <- deref enqueued
      unless enqueued' $ emit_ pendRequest req

  taskModuleDef $ incl sendRequest

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
