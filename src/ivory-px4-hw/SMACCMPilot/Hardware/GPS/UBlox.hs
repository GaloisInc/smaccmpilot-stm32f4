{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Hardware.GPS.UBlox
  ( ubloxGPSTask
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

ubloxGPSTask :: ChannelSource 1024 (Stored Uint8)
             -> ChannelSink   128 (Stored Uint8)
             -> Task p ()
ubloxGPSTask isrc osnk = do
  istream <- withChannelEmitter isrc "istream"
  ostream <- withChannelEvent   osnk "ostream"
  return ()
