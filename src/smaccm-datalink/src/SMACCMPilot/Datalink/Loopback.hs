{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Datalink.Loopback
  ( frame_loopback
  ) where

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.RingBuffer

import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Ivory.Types.SymmetricKey
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Tower
import SMACCMPilot.Datalink.HXStream.Tower

frame_loopback :: SymmetricKey
               -> BackpressureTransmit HXCyphertext (Stored IBool)
               -> ChanOutput (Stored Uint8)
               -> Tower e ()
frame_loopback sk o i = do
  ct_buf_in <- channel
  ct_buf_out <- channel
  airDataDecodeTower "test" i (fst ct_buf_in)
  pt_out <- commsecDecodeTower "test" (symKeySaltArrayIval (sk_c2s sk))
                                      (snd ct_buf_out)
  ct_out <- commsecEncodeTower "test" (symKeySaltArrayIval (sk_s2c sk))
                                      pt_out
  airDataEncodeTower "test" ct_out o

  p <- period (Milliseconds 10)

  monitor "buffered_ctloopback" $ do
    (rb :: RingBuffer 4 CyphertextArray) <- monitorRingBuffer "loopback"
    handler (snd ct_buf_in) "ct_in" $ do
      callback $ \v -> do
        _ <- ringbuffer_push rb v
        return ()
    handler p "periodic_pop" $ do
      e <- emitter (fst ct_buf_out) 1
      callback $ \_ -> do
        v <- local (iarray [])
        got <- ringbuffer_pop rb v
        when got $ do
          emit e (constRef v)

