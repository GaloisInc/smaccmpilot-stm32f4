
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import System.Environment (getArgs)
import System.Exit
import System.IO

import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Word
import Numeric (showHex)

import System.Hardware.Serialport


main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> startDebugger f
    _ -> error "invalid args. usage: hxstream-serial-test SERIALPORT"

-- Debugger Implementation -----------------------------------------------------

data DebuggerMode
  = Continue
  | Exit

data DebuggerState =
  DebuggerState
    { hello :: Bool -- Placeholder...
    }

emptyDebuggerState = DebuggerState False


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

debuggerLoop :: SerialPort -> DebuggerState -> IO DebuggerState
debuggerLoop p state = do
  bs <- recv p 1
  foldM processByte state (B.unpack bs)

processByte :: DebuggerState -> Char -> IO DebuggerState
processByte s c = do
  let b = fromIntegral (C.ord c) :: Word8
  putStr ("0x" ++ (showHex b " "))
  return s

