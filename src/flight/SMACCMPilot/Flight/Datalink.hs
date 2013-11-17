{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Datalink
  ( datalink ) where

import GHC.TypeLits
import Data.Char (ord)

import Ivory.Language
import Ivory.Tower

import qualified Ivory.HXStream                        as H

import qualified SMACCMPilot.Flight.Datalink.AirData   as A
import qualified SMACCMPilot.Flight.Datalink.RadioData as R
import qualified SMACCMPilot.Communications            as C

--------------------------------------------------------------------------------

datalink :: (SingI n0, SingI n1, SingI n2, SingI n3)
         => ChannelSink   n0 (Stored Uint8) -- from UART
         -> ChannelSource n1 (Stored Uint8) -- to UART
         -> Tower p ( ChannelSink   8 C.CommsecArray -- to decrypter
                    , ChannelSource 8 C.CommsecArray -- from encrypter to Hx
                    , ChannelSink   n2 (Struct "radio_stat")
                      -- XXX no endpoint currently
                    , ChannelSink   n3 (Struct "radio_info"))
datalink istream ostream = do
  framed_i <- channelWithSize
  framed_o <- channelWithSize
  stat_o   <- channelWithSize
  info_o   <- channelWithSize
  task "datalink" $ do
    decoder istream (src framed_o) (src stat_o) (src info_o)
    encoder (snk framed_i) ostream
    taskModuleDef $ depend H.hxstreamModule
  return (snk framed_o, src framed_i, snk stat_o, snk info_o)

--------------------------------------------------------------------------------

-- | Handle either airdata or radiodata messages from the UART on link_sink.
-- De-hxstream and send on the appropriate channel (to SMACCMPilot or radio data
-- channels).
decoder :: (SingI n0, SingI n1, SingI n2, SingI n3)
        => ChannelSink   n0 (Stored Uint8) -- from UART
        -> ChannelSource n1 C.CommsecArray -- to Commsec
        -> ChannelSource n2 (Struct "radio_stat") -- XXX no endpoint
        -> ChannelSource n3 (Struct "radio_info") -- XXX no endpoint
        -> Task p ()
decoder link_sink framed_src stat_src info_src = do
  link_istream   <- withChannelEvent   link_sink  "link_istream"
  framed_ostream <- withChannelEmitter framed_src "framed_ostream"
  stat_ostream   <- withChannelEmitter stat_src   "stat_ostream"
  info_ostream   <- withChannelEmitter info_src   "info_ostream"
  hx             <- taskLocalInit "hx_decoder_state" H.initStreamState
  airhandler     <- A.airDataHandler framed_ostream
  radiohandler   <- R.radioDataHandler stat_ostream info_ostream
  onEventV link_istream $ \v ->
    noReturn $ H.decodes [airhandler, radiohandler] hx v

--------------------------------------------------------------------------------

-- | Encode airdata or generated radio data to give to either the UART task.
encoder :: (SingI n0, SingI n1)
        => ChannelSink   n0 C.CommsecArray -- from commsec
        -> ChannelSource n1 (Stored Uint8) -- to UART
        -> Task p ()
encoder framed_snk link_src = do
  link_ostream   <- withChannelEmitter  link_src   "link_ostream"
  framed_istream <- withChannelEvent    framed_snk "framed_istream"
  -- Send air data as quickly as we get it
  onEvent framed_istream $ \frame -> noReturn $
    H.encode C.airDataTag frame (emitV_ link_ostream)
  -- Periodically send binary info request to radio.
  onPeriod 1000 $ \_t -> noReturn $ do
    (frame :: Ref (Stack s) (Array 2 (Stored Uint8))) <- local $ iarray
      [ ival (charUint8 'B')
      , ival (charUint8 '\r')
      ]
    H.encode C.radioDataTag (constRef frame) (emitV_ link_ostream)
  where
  charUint8 :: Char -> Uint8
  charUint8 = fromIntegral . ord

--------------------------------------------------------------------------------
