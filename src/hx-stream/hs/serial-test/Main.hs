
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Environment (getArgs)
import System.Exit
import System.IO

import qualified Data.ByteString as B
import qualified Data.Char as C
import Data.Word
import Numeric (showHex)

import System.Hardware.Serialport
import Data.HXStream


main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> startDebugger f
    _ -> error "invalid args. usage: hxstream-serial-test SERIALPORT"

-- Debugger Implementation -----------------------------------------------------

data DebuggerMode
  = Continue
  | Send
  | Exit

startDebugger :: FilePath -> IO ()
startDebugger f = do
  mode <- newIORef Continue
  forkIO (runDebugger mode f)
  hSetBuffering stdin NoBuffering
  controlMode mode
  where
  controlMode dbgsignal = do
    c <- getChar
    case C.toLower c of
     'q' -> do writeIORef dbgsignal Exit
               exitSuccess
     's' -> do writeIORef dbgsignal Send
               controlMode dbgsignal
     _ -> controlMode dbgsignal

runDebugger :: IORef DebuggerMode -> FilePath -> IO ()
runDebugger mode port = do
  serial <- openSerial port defaultSerialSettings { commSpeed = CS57600 }
  loop mode serial emptyStreamState
  where
  loop mode serial state = do
    state' <- debuggerLoop serial state
    m <- readIORef mode
    let cont = loop mode serial state'
    case m of
      Continue -> cont
      Send -> do
        writeIORef mode Continue
        putStrLn "Sending Frame"
        putHexBS e
        send serial e
        cont
      Exit -> closeSerial serial

  e = encode (0:payload)
  payload = [1,2,3,0x7b,0x7c,0x7d,0x7e,9]
  -- f = take 128 (0:[127,128..])

putHex :: Word8 -> IO ()
putHex b = putStr ("0x" ++ (showHex b " "))

putHexBS :: B.ByteString -> IO ()
putHexBS s = mapM_ putHex (B.unpack s) >> putStrLn ""

debuggerLoop :: SerialPort -> StreamState -> IO StreamState
debuggerLoop p state = do
  bs <- recv p 1
  foldM processByte state (B.unpack bs)

processByte :: StreamState -> Word8 -> IO StreamState
processByte s b = do
  let s' = decodeSM b s
  putHex b
  case completeFrame s' of
    Just f -> do
      putStr " /fr/\n"
      putFrame f
      return emptyStreamState
    _ -> return s'

putFrame :: [Word8] -> IO ()
putFrame f = putStr "\n> " >> mapM_ putHex f

