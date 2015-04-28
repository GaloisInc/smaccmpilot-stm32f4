
module SMACCMPilot.Datalink.Client.Repl where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Word

import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Pipes

import System.Console.Haskeline

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.ByteString
import SMACCMPilot.Datalink.Client.Pipes

import qualified SMACCMPilot.Datalink.HXStream.Native as HX
import SMACCMPilot.Commsec.Sizes

data ReplCmd
  = SendRaw ByteString
  | SendHXFrame  Word8 ByteString
  | SendAirCyphertext ByteString

replClient :: Options -> IO ()
replClient opts = do
  console <- newConsole opts

  (ser_in_pop, ser_out_push) <- serialServer opts console

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> forever (await >> return ())

  (repl_push, repl_pop) <- newQueue

  _ <- asyncRunEffect console "serial out"
           $ popProducer repl_pop
         >-> replTranslator
         >-> bytestringLog "raw"
         >-> pushConsumer ser_out_push

  runInputT defaultSettings $ runEffect $
          repl console >-> pushConsumer repl_push
  where
  repl :: Console -> Producer ReplCmd (InputT IO) ()
  repl console = do
    -- give concurrent threads time to print to console
    lift $ lift $ threadDelay 100 
    c <- lift $ lift $ getConsoleOutput console
    lift $ outputStr c
    minput <- lift (getInputLine "% ")
    case minput of
      Just ('c':'t':' ':ls) -> loop $ yield (SendAirCyphertext (B.pack ls))
      Just ('r':'a':'w':' ':ls) -> loop $ yield (SendRaw (B.pack ls))
      Just "help" -> loop $ lift $ outputStr $ unlines
        [ "raw TEXT            : send arbitrary ascii over the serial port"
        , "frame TEXT          : send hxframed ascii over the serial port"
        , "log [quiet|verbose] : set logging level"
        , "help                : show this message"
        , "quit                : exit"
        ]

      Just "log verbose" -> repl (consoleWithLogLevel console 2)
      Just "log"         -> repl (consoleWithLogLevel console 1)
      Just "log quiet"   -> repl (consoleWithLogLevel console 0)

      Just "quit" -> return ()
      Just _    -> repl console
      Nothing   -> repl console
    where
    loop k = k >> repl console

replTranslator :: Pipe ReplCmd ByteString GW ()
replTranslator = await >>= go >> replTranslator
  where
  go (SendRaw bs) = yield bs
  go (SendAirCyphertext bs) =
    case bytestringPad cyphertextSize bs of
      Left err -> lift (writeErr ("when padding to cyphertextSize: " ++ err))
      Right padded -> go (SendHXFrame 0 padded)
  go (SendHXFrame t bs) = go (SendRaw (HX.encode t bs))
