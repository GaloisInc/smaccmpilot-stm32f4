
module SMACCMPilot.Datalink.Client where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Control.Concurrent.Async (wait)
import           System.IO (stderr)
import           Pipes

import System.Console.Haskeline

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial

client :: Options -> IO ()
client opts = do
  console <- newConsole opts stderr

  (ser_out', ser_out) <- newQueue
  (ser_in, ser_in')   <- newQueue

  serialServer opts console ser_out' ser_in'

  a <- asyncRun "serial_in" $ runInputT defaultSettings $ runEffect $
        cmdinput >-> pushConsumer ser_in

  wait a

cmdinput :: Producer ByteString (InputT IO) ()
cmdinput = do
  minput <- lift (getInputLine "% ")
  case minput of
    Just ":q" -> return ()
    Just l    -> yield (B.pack l) >> cmdinput
    Nothing   -> cmdinput

