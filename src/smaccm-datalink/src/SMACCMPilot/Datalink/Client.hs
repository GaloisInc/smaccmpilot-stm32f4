
module SMACCMPilot.Datalink.Client where

import Pipes
import System.Exit

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Pipes
import SMACCMPilot.Datalink.Mode

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B (w2c)
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.Sizes


datalinkClient :: Options
               -> DatalinkMode
               -> (Pushable ByteString -> Poppable ByteString -> Console -> IO ())
               -> IO ()
datalinkClient opts dmode client = case dmode of
  PlaintextMode ->
    aux (aRunPipe "unpadder" (unpadder plaintextSize))
        (aRunPipe "padder" (padder cyphertextSize))
  SymmetricCommsecMode DatalinkClient sk ->
    aux (aRunPipe "sym decoder" (commsecDecoder (keyToBS (sk_s2c sk))))
        (aRunPipe "sym encoder" (commsecEncoder (keyToBS (sk_c2s sk))))
  SymmetricCommsecMode _ _ -> error "not implementing symmetric commsec datalink server right now"
  KeyExchangeMode _ _ _ _ -> error "key exchange mode unsupported"
  where
  aux decoder encoder = do
    putStrLn ("Datalink client starting in " ++ mode)
    console <- newConsolePrinter opts

    (ser_in_pop, ser_out_push) <- serialServer opts console

    (ct_out_frame_push, ct_out_frame_pop) <- newQueue
    (pt_unpad_out_frame_push, pt_unpad_out_frame_pop) <- newQueue
    (pt_out_frame_push, pt_out_frame_pop) <- newQueue
    (ct_in_frame_push, ct_in_frame_pop) <- newQueue
    (pt_in_frame_push, pt_in_frame_pop) <- newQueue

    i <- aRunPipe "serial in" ser_in_pipe console
          ser_in_pop ct_in_frame_push

    d <- decoder console ct_in_frame_pop pt_in_frame_push

    p <- aRunPipe "pt padder" (padder plaintextSize) console
          pt_unpad_out_frame_pop pt_out_frame_push

    e <- encoder console pt_out_frame_pop ct_out_frame_push

    o <- aRunPipe "serial out" ser_out_pipe console
          ct_out_frame_pop ser_out_push

    client pt_unpad_out_frame_push pt_in_frame_pop console
    mapM_ wait [i, d, p, e, o]
    exitSuccess

  aRunPipe name p console pop_chan push_chan =
    asyncRunEffect console name $
      popProducer pop_chan >-> p >-> pushConsumer push_chan

  ser_out_pipe = tagger 0
             >-> frameLog
             >-> hxEncoder
             >-> bytestringLog "raw"

  ser_in_pipe = bytestringLog "raw"
            >-> hxDecoder
            >-> frameLog
            >-> untagger 0

  keyToBS = B.pack . map B.w2c
  mode = case dmode of
    PlaintextMode -> "plaintext mode"
    SymmetricCommsecMode DatalinkClient _ ->
      "symmetric commsec client mode"
    SymmetricCommsecMode DatalinkServer _ ->
      "symmetric commsec server mode"
    KeyExchangeMode DatalinkClient _ _ _ ->
      "key exchange commsec client mode"
    KeyExchangeMode DatalinkServer _ _ _ ->
      "key exchange commsec server mode"

