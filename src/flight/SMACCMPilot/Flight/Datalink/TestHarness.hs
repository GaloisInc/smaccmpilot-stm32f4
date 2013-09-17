{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink.TestHarness
  ( frameLoopback
  ) where

import Ivory.Language
import Ivory.Tower

frameLoopback :: ChannelSink   2 (Array 128 (Stored Uint8))
              -> ChannelSource 2 (Array 128 (Stored Uint8))
              -> Tower p ()
frameLoopback isrc osnk = task "datalinkFrameLoopback" $ do
  istream <- withChannelEvent   isrc "istream"
  ostream <- withChannelEmitter osnk "ostream"
  onEvent istream $ \f -> do
    o <- local (iarray [])
    refCopy o f
    a <- deref (f ! 0)
    b <- deref (f ! 1)
    store (o ! 2) (a + b)
    emit_ ostream (constRef o)
  return ()

