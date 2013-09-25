
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
    [f] -> do
      r <- newIORef Continue
      startDebugger r f
    _ -> error "invalid args. usage: hxstream-serial-test SERIALPORT"

-- Debugger Implementation -----------------------------------------------------

data DebuggerMode
  = Continue
  | Send [Word8]
  | Exit

startDebugger :: IORef DebuggerMode -> FilePath -> IO ()
startDebugger sig f = do
  forkIO (runDebugger sig f)
  hSetBuffering stdin NoBuffering
  loop
  where
  loop = do
    c <- getChar
    case C.toLower c of
     'q' -> writeIORef sig Exit >> exitSuccess
     '0' -> sendpacket (radiocmd "ATI0") >> loop
     '1' -> sendpacket (radiocmd "ATI0") >> loop
     '2' -> sendpacket (radiocmd "ATI2") >> loop
     '3' -> sendpacket (radiocmd "ATI2") >> loop
     '4' -> sendpacket (radiocmd "ATI4") >> loop
     '5' -> sendpacket (radiocmd "ATI5") >> loop
     '6' -> sendpacket (radiocmd "ATI6") >> loop
     '7' -> sendpacket (radiocmd "ATI7") >> loop
     's' -> sendpacket (airdata packet1) >> loop
     'd' -> sendpacket (airdata packet2) >> loop
     'f' -> sendpacket (airdata packet3) >> loop
     'b' -> sendpacket (radiocmd "B1") >> loop
     'n' -> sendpacket (radiocmd "B2") >> loop
     'e' -> sendpacket [] >> loop
     _ -> loop
  sendpacket p = writeIORef sig (Send p)
  airdata p = 0:p
  packet1 = [1,2,0xFF,0x7b,0x7c,0x7d,0x7e,9]
  packet2 = [5,6]
  packet3 = take 96 [0..]

radiocmd :: String -> [Word8]
radiocmd s = 1 : (map (fromIntegral . C.ord) (s ++ "\r"))

runDebugger :: IORef DebuggerMode -> FilePath -> IO ()
runDebugger sig port = do
  serial <- openSerial port defaultSerialSettings { commSpeed = CS57600 }
  loop serial emptyStreamState
  where
  loop serial state = do
    state' <- debuggerLoop serial state
    m <- readIORef sig
    let cont = loop serial state'
    case m of
      Continue -> cont
      Send payload -> do
        writeIORef sig Continue
        putStrLn "Sending Frame"
        let packetBS = encode payload
        putHexBS packetBS
        send serial packetBS
        cont
      Exit -> closeSerial serial

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
  case completeFrame s' of
    Just f -> do
      putFrame f
      return emptyStreamState
    _ -> return s'

putFrame :: [Word8] -> IO ()
putFrame f = do
  putStr "\n> "
  mapM_ putHex f
  putStr "\n"
  case f of
    1:rest -> putFrameString rest
    _ -> return ()

putFrameString :: [Word8] -> IO ()
putFrameString f = print $ map (C.chr . fromIntegral) f

