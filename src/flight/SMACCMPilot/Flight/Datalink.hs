{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Datalink
  ( datalink
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import Ivory.HXStream

datalink :: (SingI n, SingI m, SingI nn, SingI mm)
         => ChannelSink   n (Stored Uint8)
         -> ChannelSource m (Stored Uint8)
         -> Tower p ( ChannelSink   nn (Array 128 (Stored Uint8))
                    , ChannelSource mm (Array 128 (Stored Uint8)))
datalink dlinksink dlinksrc = do --  istream ostream = do
  framed_i <- channelWithSize
  framed_o <- channelWithSize
  task "datalink" $ do
    decoder dlinksink (src framed_o)
    encoder (snk framed_i) dlinksrc
    taskModuleDef $ do
      depend hxstream_types
  addModule hxstream_types
  return (snk framed_o, src framed_i)

tag :: Uint8
tag = 0

decoder :: (SingI n, SingI m)
        => ChannelSink   n (Stored Uint8)
        -> ChannelSource m (Array 128 (Stored Uint8))
        -> Task p ()
decoder link_sink framed_src = do
    link_istream   <- withChannelEvent   link_sink  "link_istream"
    framed_ostream <- withChannelEmitter framed_src "framed_ostream"
    hx             <- taskLocalInit "hx_decoder_state" hx_ival
    decoded        <- taskLocal     "decoded"
    decodedCtr     <- taskLocalInit "frames_decoded" (ival (0::Uint32))
    let handler = FrameHandler
          { fh_tag = tag
          , fh_begin = return ()
          , fh_data = \v offs -> store (decoded ! (toIx offs)) v
          , fh_end = do
              emit_ framed_ostream (constRef decoded)
              decodedCtr %= (+1)
          }
    onEventV link_istream $ \v -> noReturn $ decodeSM [handler] hx v

encoder :: (SingI n, SingI m)
        => ChannelSink   m (Array 128 (Stored Uint8))
        -> ChannelSource n (Stored Uint8)
        -> Task p ()
encoder framed_snk link_src = do
  link_ostream   <- withChannelEmitter  link_src   "link_ostream"
  framed_istream <- withChannelEvent    framed_snk "framed_ostream"
  onEvent framed_istream $ \frame -> noReturn $ encodeToBS tag frame (emitV_ link_ostream)

