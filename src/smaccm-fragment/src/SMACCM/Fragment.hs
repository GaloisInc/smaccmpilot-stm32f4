{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCM.Fragment where

import Ivory.BSP.STM32.Driver.CAN
import Ivory.Language
import Ivory.Serialize
import Ivory.Tower
import Numeric

fragmentSender :: forall len a e
                . (ANat len, IvoryArea a, IvoryZero a, Packable a)
               => Int
               -> Bool
               -> CANTransmitAPI
               -> Proxy len
               -> Tower e (ChanInput a, ChanInput (Stored IBool), ChanOutput (Stored IBool))
fragmentSender baseID ide tx Proxy = do
  (reqChan, reqSrc) <- channel
  (abortChan, abortSrc) <- channel
  (resDst, resChan) <- channel

  let idstr = "0x" ++ showHex baseID ""
  monitor ("fragment_" ++ idstr) $ do
    sent <- stateInit ("fragment_sent_" ++ idstr) (izero :: Init (Stored Uint8))
    aborting <- state ("fragment_aborting_" ++ idstr)
    buf <- stateInit ("fragment_buf_" ++ idstr) (izero :: Init (Array len (Stored Uint8)))

    let rep = packRep
    if packSize rep /= arrayLen buf then fail $ "wrong buffer size " ++ show (arrayLen buf) ++ " given for CAN ID " ++ idstr ++ ": should be " ++ show (packSize rep) else return ()

    let sendFragment idx = do
          let remaining_len = arrayLen buf - 8 * idx
          let len = (remaining_len >? 8) ? (8, remaining_len)
          msg <- local $ istruct
            [ tx_id .= ival (fromIntegral baseID + safeCast idx)
            , tx_ide .= ival (if ide then true else false)
            , tx_rtr .= ival false
            , tx_len .= ival (toIx len)
            ]
          for (toIx len) $ \ i -> refCopy (msg ~> tx_buf ! i) (buf ! toIx (safeCast idx * 8 + fromIx i))
          store sent (idx + 1)
          return msg

    handler reqSrc ("fragment_req_" ++ idstr) $ do
      txReq <- emitter (canTXReq tx) 1
      callback $ \ req -> do
        was_sent <- deref sent
        assert $ was_sent ==? 0

        packInto' rep buf 0 req
        msg <- sendFragment 0
        emit txReq $ constRef msg

    handler (canTXRes tx) ("fragment_complete_" ++ idstr) $ do
      txReq <- emitter (canTXReq tx) 1
      res <- emitter resDst 1
      callbackV $ \ success -> do
        let finished v = do
              emitV res v
              store sent 0
              store aborting false

        ifte_ (iNot success) (finished false) $ do
          already_sent <- deref sent
          assert $ already_sent >? 0

          ifte_ (arrayLen buf <=? 8 * (safeCast already_sent :: Uint16)) (finished true) $ do
            should_abort <- deref aborting
            ifte_ should_abort (finished false) $ do
              msg <- sendFragment already_sent
              emit txReq $ constRef msg

    handler abortSrc ("fragment_abort_" ++ idstr) $ do
      txAbort <- emitter (canTXAbortReq tx) 1
      callback $ const $ do
        store aborting true
        emitV txAbort true

  return (reqChan, abortChan, resChan)
