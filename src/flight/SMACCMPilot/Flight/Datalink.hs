{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Datalink
  ( datalink ) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Tower

import qualified Ivory.HXStream                        as H

import qualified SMACCMPilot.Flight.Datalink.AirData   as A
import qualified SMACCMPilot.Flight.Datalink.RadioData as R
import qualified SMACCMPilot.Communications            as C

--------------------------------------------------------------------------------

datalink :: String
         -> ChannelSink   (Stored Uint8) -- from UART
         -> ChannelSource (Stored Uint8) -- to UART
         -> Tower p ( ChannelSink   C.CommsecArray -- to decrypter
                    , ChannelSource C.CommsecArray -- from encrypter to Hx
                    , ChannelSink   (Struct "radio_stat")
                      -- XXX no endpoint currently
                    , ChannelSink   (Struct "radio_info"))
datalink name istream ostream = do
  framed_i <- channel
  framed_o <- channel
  stat_o   <- channel
  info_o   <- channel
  task ("datalink_" ++ name) $ do
    decoder istream (src framed_o) (src stat_o) (src info_o)
    encoder (snk framed_i) ostream
    taskModuleDef $ depend H.hxstreamModule
  return (snk framed_o, src framed_i, snk stat_o, snk info_o)

--------------------------------------------------------------------------------

-- | Handle either airdata or radiodata messages from the UART on link_sink.
-- De-hxstream and send on the appropriate channel (to SMACCMPilot or radio data
-- channels).
decoder :: ChannelSink   (Stored Uint8) -- from UART
        -> ChannelSource C.CommsecArray -- to Commsec
        -> ChannelSource (Struct "radio_stat") -- XXX no endpoint
        -> ChannelSource (Struct "radio_info") -- XXX no endpoint
        -> Task p ()
decoder link_sink framed_src stat_src info_src = do
  link_istream   <- withChannelEvent   link_sink  "link_istream"
  framed_ostream <- withChannelEmitter framed_src "framed_ostream"
  stat_ostream   <- withChannelEmitter stat_src   "stat_ostream"
  info_ostream   <- withChannelEmitter info_src   "info_ostream"
  hx             <- taskLocalInit "hx_decoder_state" H.initStreamState
  airhandler     <- A.airDataHandler framed_ostream
  radiohandler   <- R.radioDataHandler stat_ostream info_ostream
  handleV link_istream "link_istream" $ \v ->
    noReturn $ H.decodes [airhandler, radiohandler] hx v

--------------------------------------------------------------------------------

-- | Encode airdata or generated radio data to give to either the UART task.
encoder :: ChannelSink   C.CommsecArray -- from commsec
        -> ChannelSource (Stored Uint8) -- to UART
        -> Task p ()
encoder framed_snk link_src = do
  link_ostream   <- withChannelEmitter  link_src   "link_ostream"
  framed_istream <- withChannelEvent    framed_snk "framed_istream"
  -- Send air data as quickly as we get it
  handle framed_istream "framed_istream" $ \frame -> noReturn $
    H.encode C.airDataTag frame (emitV_ link_ostream)
  -- Periodically send binary info request to radio.
  onPeriod (Milliseconds 1000) $ \_t -> noReturn $ do
    (frame :: Ref (Stack s) (Array 2 (Stored Uint8))) <- local $ iarray
      [ ival (charUint8 'B')
      , ival (charUint8 '\r')
      ]
    H.encode C.radioDataTag (constRef frame) (emitV_ link_ostream)
  where
  charUint8 :: Char -> Uint8
  charUint8 = fromIntegral . ord

--------------------------------------------------------------------------------
