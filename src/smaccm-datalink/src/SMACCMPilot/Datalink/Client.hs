
module SMACCMPilot.Datalink.Client where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Word
import           Text.Printf

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

import SMACCMPilot.Datalink.HXStream.Native
import SMACCMPilot.Commsec.Sizes


data ReplCmd
  = SendRaw ByteString
  | SendHXFrame  Word8 ByteString
  | SendAirCyphertext ByteString

client :: Options -> IO ()
client opts = do
  console <- newConsole opts

  (ser_out_push, ser_out_pop) <- newQueue
  (ser_in_push, ser_in_pop)   <- newQueue


  serialServer opts console ser_in_push ser_out_pop
  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
--        >-> word8Log ""
        >-> hxdecoder
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

word8Log :: String -> Pipe Word8 Word8 GW ()
word8Log tag = do
  w8 <- await
  lift $ writeLog (printf "%s Word8 %d (0x%0.2x)" tag w8 w8)
  word8Log tag

frameLog :: Pipe (Tag, ByteString) (Tag, ByteString) GW ()
frameLog = do
  (t, bs) <- await
  lift $ writeLog $ bytestringDebug (printf "frame (tag %d)" t) bs
  yield (t, bs)
  frameLog

bytestringLog :: String -> Pipe ByteString ByteString GW ()
bytestringLog tag = do
  bs <- await
  lift $ writeLog $ bytestringDebug tag bs
  yield bs
  bytestringLog tag

replTranslator :: Pipe ReplCmd ByteString GW ()
replTranslator = await >>= go >> replTranslator
  where
  go (SendRaw bs) = yield bs
  go (SendAirCyphertext bs) =
    case bytestringPad cyphertextSize bs of
      Left err -> lift (writeErr ("when padding to cyphertextSize: " ++ err))
      Right padded -> go (SendHXFrame 0 padded)
  go (SendHXFrame t bs) = go (SendRaw (encode t bs))

hxdecoder :: Monad m => Pipe Word8 (Tag, ByteString) m ()
hxdecoder = aux emptyStreamState
  where
  aux ss = do
     b <- await
     let (mf, ss') = decodeByte b ss
     case mf of
       Just tbs -> yield tbs
       Nothing -> return ()
     aux ss'

