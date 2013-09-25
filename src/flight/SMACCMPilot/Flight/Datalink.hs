{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Datalink
  ( datalink
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Tower
import Ivory.HXStream

import SMACCMPilot.Flight.Datalink.AirData
import SMACCMPilot.Flight.Datalink.RadioData

datalink :: (SingI n, SingI m, SingI nn, SingI mm, SingI oo, SingI pp)
         => ChannelSink   n (Stored Uint8)
         -> ChannelSource m (Stored Uint8)
         -> Tower p ( ChannelSink   nn AirDataFrame
                    , ChannelSource mm AirDataFrame
                    , ChannelSink   oo (Struct "radio_stat")
                    , ChannelSink   pp (Struct "radio_info"))
datalink istream ostream = do
  framed_i <- channelWithSize
  framed_o <- channelWithSize
  stat_o   <- channelWithSize
  info_o   <- channelWithSize
  task "datalink" $ do
    decoder istream (src framed_o) (src stat_o) (src info_o)
    encoder (snk framed_i) ostream
    taskModuleDef $ depend hxstream_types
  addModule hxstream_types
  return (snk framed_o, src framed_i, snk stat_o, snk info_o)

decoder :: (SingI n, SingI m, SingI o, SingI q)
        => ChannelSink   n (Stored Uint8)
        -> ChannelSource m AirDataFrame
        -> ChannelSource o (Struct "radio_stat")
        -> ChannelSource q (Struct "radio_info")
        -> Task p ()
decoder link_sink framed_src stat_src info_src = do
  link_istream   <- withChannelEvent   link_sink  "link_istream"
  framed_ostream <- withChannelEmitter framed_src "framed_ostream"
  stat_ostream   <- withChannelEmitter stat_src   "stat_ostream"
  info_ostream   <- withChannelEmitter info_src   "info_ostream"
  hx             <- taskLocalInit "hx_decoder_state" hx_ival

  airhandler     <- airDataHandler framed_ostream
  radiohandler   <- radioDataHandler stat_ostream info_ostream
  onEventV link_istream $ \v -> noReturn $ decodeSM [airhandler, radiohandler] hx v

encoder :: (SingI n, SingI m)
        => ChannelSink   m AirDataFrame
        -> ChannelSource n (Stored Uint8)
        -> Task p ()
encoder framed_snk link_src = do
  link_ostream   <- withChannelEmitter  link_src   "link_ostream"
  framed_istream <- withChannelEvent    framed_snk "framed_ostream"
  onEvent framed_istream $ \frame -> noReturn $ encodeToBS airDataTag frame (emitV_ link_ostream)

