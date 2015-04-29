
module SMACCMPilot.Datalink.Client.Console
  ( Console
  , consoleLog
  , consoleError
  , consoleDebug
  , newConsole
  , newConsolePrinter
  , getConsoleOutput
  , annotate
  , consoleWithLogLevel
  ) where

import qualified Control.Concurrent.Async        as A
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           System.IO

import SMACCMPilot.Datalink.Client.Opts

data MsgTag = ErrorMsg
            | LogMsg
            | DebugMsg

type Impl = TVar [(MsgTag, String)]

data Console = RealConsole Options Impl
             | AnnotatedConsole String Console

consoleImpl :: Console -> Impl
consoleImpl (RealConsole _ i) = i
consoleImpl (AnnotatedConsole _ q) = consoleImpl q

consoleOpts :: Console -> Options
consoleOpts (RealConsole o _) = o
consoleOpts (AnnotatedConsole _ q) = consoleOpts q

consoleWithOpts :: Console -> Options -> Console
consoleWithOpts (RealConsole _ i) o' = RealConsole o' i
consoleWithOpts (AnnotatedConsole s c) o' = AnnotatedConsole s (consoleWithOpts c o')

consoleWithLogLevel :: Console -> Integer -> Console
consoleWithLogLevel c l = consoleWithOpts c (consoleOpts c) { logLevel = l }

annotate :: Console -> String -> Console
annotate c s = AnnotatedConsole (s ++ ": ") c

consoleLog :: Console -> String -> IO ()
consoleLog console s = writeConsole console LogMsg s

consoleError :: Console -> String -> IO ()
consoleError console s = writeConsole console ErrorMsg s

consoleDebug :: Console -> String -> IO ()
consoleDebug console s = writeConsole console DebugMsg s

writeConsole :: Console -> MsgTag -> String -> IO ()
writeConsole (RealConsole _ q) t m = void $ atomically $ modifyTVar' q (\l -> (t,m):l)
writeConsole (AnnotatedConsole s c) t m = writeConsole c t (s++m)

newConsole :: Options -> IO Console
newConsole opts = do
  q <- newTVarIO []
  return $ RealConsole opts q

newConsolePrinter :: Options -> IO Console
newConsolePrinter opts = do
  c <- newConsole opts
  let run = do
        threadDelay 10000
        o <- getConsoleOutput c
        case o of
          [] -> return ()
          _ -> putStrLn o
  void $ A.async $ catch (forever run) exit
  return c
  where
  exit :: SomeException -> IO ()
  exit x = hPutStrLn stderr ("console printer exited with exception: " ++ show x)

getConsoleOutput :: Console -> IO String
getConsoleOutput c = do
  ls <- atomically $ do
          l <- readTVar q
          writeTVar q []
          return l
  return (unlines (map showLog (filter p (reverse ls))))
  where
  q = consoleImpl c

  llevel = logLevel (consoleOpts c)
  p (ErrorMsg,_) = llevel > 0
  p (LogMsg,_)   = llevel > 1
  p (DebugMsg,_) = llevel > 2

  showLog (t,msg) = case t of
    ErrorMsg -> "ERR: " ++ msg
    LogMsg ->  "LOG: " ++ msg
    DebugMsg -> "DBG: " ++ msg

