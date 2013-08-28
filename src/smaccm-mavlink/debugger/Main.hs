
import System.Environment (getArgs)
import qualified Data.Char as C
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.IO
import System.Exit

import qualified Data.ByteString as B
import System.Hardware.Serialport

import Debugger

data DebuggerMode
  = Continue Verboseness
  | Exit
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> startDebugger f
    _ -> error "invalid args. usage: smaccm-mavlink-debugger SERIALPORT"

startDebugger :: FilePath -> IO ()
startDebugger f = do
  mode <- newIORef (Continue Chatty)
  forkIO (runDebugger mode f)
  hSetBuffering stdin NoBuffering
  controlMode mode
  where
  controlMode dbgsignal = do
    let continue = controlMode dbgsignal
    c <- getChar
    case c of
     'q' -> do writeIORef dbgsignal Exit
               -- exitSuccess
     'v' -> do writeIORef dbgsignal (Continue Chatty)
               continue
     'V' -> do writeIORef dbgsignal (Continue Quiet)
               continue
     _ -> continue

runDebugger :: IORef DebuggerMode -> FilePath -> IO ()
runDebugger mode port = do
  serial <- openSerial port defaultSerialSettings { commSpeed = CS57600 }
  let loop state = do
        m <- readIORef mode
        case m of
          Continue v ->  do
            bs <- recv serial 1
            state' <- debuggerLoop (B.unpack bs) state v
            loop state'
          Exit -> closeSerial serial
  loop (emptyDebuggerState, [])

