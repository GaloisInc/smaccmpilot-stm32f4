
import System.Environment (getArgs)
import qualified Data.Char as C
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport

import Debugger

data DebuggerMode
  = Continue
  | Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> startDebugger f
    _ -> error "invalid args. usage: smaccm-mavlink-debugger SERIALPORT"

startDebugger :: FilePath -> IO ()
startDebugger f = do
  mode <- newMVar Continue
  forkIO (runDebugger mode f)
  hSetBuffering stdin NoBuffering
  controlMode mode
  where
  controlMode dbgsignal = do
    c <- getChar
    case C.toLower c of
     'q' -> do putMVar dbgsignal Exit
               exitSuccess
     _ -> controlMode dbgsignal

runDebugger :: MVar DebuggerMode -> FilePath -> IO ()
runDebugger mode port = do
  serial <- openSerial port defaultSerialSettings { commSpeed = CS57600 }
  loop mode serial emptyDebuggerState
  where
  loop mode serial state = do
    state' <- debuggerLoop serial state
    m <- readMVar mode
    case m of
      Continue -> loop mode serial state'
      Exit -> closeSerial serial
