
module SMACCMPilot.Datalink.Client.Console
  ( Console
  , consoleLog
  , consoleError
  , consoleDebug
  , newConsole
  , annotate
  ) where

import System.IO
import qualified Control.Concurrent            as C
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.STM

import SMACCMPilot.Datalink.Client.Opts

data MsgTag = ErrorMsg
            | LogMsg
            | DebugMsg

data Console = RealConsole (TQueue (MsgTag, String))
             | AnnotatedConsole String Console

annotate :: Console -> String -> Console
annotate c s = AnnotatedConsole (s ++ ": ") c

consoleLog :: Console -> String -> IO ()
consoleLog console s = writeConsole console LogMsg s

consoleError :: Console -> String -> IO ()
consoleError console s = writeConsole console ErrorMsg s

consoleDebug :: Console -> String -> IO ()
consoleDebug console s = writeConsole console DebugMsg s

writeConsole :: Console -> MsgTag -> String -> IO ()
writeConsole (RealConsole q) t m = void $ atomically $ writeTQueue q (t,m)
writeConsole (AnnotatedConsole s c) t m = writeConsole c t (s++m)

newConsole :: Options -> Handle -> IO Console
newConsole opts h = do
  q <- newTQueueIO
  _ <- C.forkIO $ printerThread q opts h
  return $ RealConsole q

printerThread :: TQueue (MsgTag, String) -> Options -> Handle -> IO ()
printerThread q opts h = forever $ do
  (t, msg) <- atomically $ readTQueue q
  case t of
    ErrorMsg -> when (llevel > 0) $
      hPutStrLn h ("ERR: " ++ msg)
    LogMsg -> when (llevel > 1)   $
      hPutStrLn h ("LOG: " ++ msg)
    DebugMsg -> when (llevel > 2) $
      hPutStrLn h ("DBG: " ++ msg)
  where
  llevel = logLevel opts

