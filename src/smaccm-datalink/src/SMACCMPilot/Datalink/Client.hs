
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
datalinkClient opts dmode client = do
  putStrLn ("Datalink client starting in " ++ mode)
  console <- newConsolePrinter opts

  (ser_in_pop, ser_out_push) <- serialServer opts console

  (out_frame_push, out_frame_pop) <- newQueue
  (in_frame_push, in_frame_pop) <- newQueue

  b <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> bytestringLog "raw"
        >-> hxDecoder
        >-> frameLog
        >-> untagger 0
        >-> case dmode of
              PlaintextMode -> cat -- XXX unpad?
              SymmetricCommsecMode sk -> commsecDecoder (keyToBS (sk_s2c sk))
        >-> pushConsumer in_frame_push

  a <- asyncRunEffect console "serial out"
           $ popProducer out_frame_pop
         >-> padder plaintextSize
         >-> case dmode of
              PlaintextMode -> padder cyphertextSize
              SymmetricCommsecMode sk -> commsecEncoder (keyToBS (sk_c2s sk))
         >-> tagger 0
         >-> frameLog
         >-> hxEncoder
         >-> bytestringLog "raw"
         >-> pushConsumer ser_out_push

  client out_frame_push in_frame_pop console
  wait a
  wait b
  exitSuccess
  where
  keyToBS = B.pack . map B.w2c
  mode = case dmode of
    PlaintextMode -> "plaintext mode"
    SymmetricCommsecMode _ -> "symmetric commsec mode"

