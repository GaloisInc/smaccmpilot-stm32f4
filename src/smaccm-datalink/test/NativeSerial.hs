
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
import SMACCMPilot.Datalink.HXStream.Native


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
  | Send Word8 [Word8] -- Tag and payload
  | Exit

airDataTag, radioCmdTag :: Word8
airDataTag  = 0
radioCmdTag = 1

startDebugger :: IORef DebuggerMode -> FilePath -> IO ()
startDebugger sig f = do
  void $ forkIO (runDebugger sig f)
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
     'b' -> sendpacket (radiocmd "B") >> loop
     'e' -> sendpacket (Send 7 []) >> loop  -- Just for testing
     _ -> loop
  sendpacket = writeIORef sig
  airdata p  = Send airDataTag p
  packet1 = [1,2,0xFF,0x7b,0x7c,0x7d,0x7e,9]
  packet2 = [5,6]
  packet3 = take 96 [0..]

radiocmd :: String -> DebuggerMode
radiocmd s = Send radioCmdTag (map (fromIntegral . C.ord) (s ++ "\r"))

runDebugger :: IORef DebuggerMode -> FilePath -> IO ()
runDebugger sig port = do
  serial <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  loop serial emptyStreamState
  where
  loop serial state = do
    state' <- debuggerLoop serial state
    m <- readIORef sig
    let cont = loop serial state'
    case m of
      Continue -> cont
      Send tag payload -> do
        writeIORef sig Continue
        putStrLn ( "Sending Frame.  Tag: " ++ show tag
                ++ " payload: " ++ show payload)
        let packetBS = encode tag (B.pack payload)
        putHexBS packetBS
        void $ send serial packetBS
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
  when (fstate s' == FrameTag) $ do
    case ftag s of
      Nothing -> return ()
      Just t  -> putFrame (t, extractFrame s)
  return s'

putFrame :: (Word8,B.ByteString) -> IO ()
putFrame (tag,fr) = do
  putStr "\n> "
  let fr' = B.unpack fr
  mapM_ putHex (tag : fr')
  putStr "\n"
  case tag of
    _ | tag == radioCmdTag -> putFrameString fr'
      | otherwise          -> return ()

putFrameString :: [Word8] -> IO ()
putFrameString = print . map (C.chr . fromIntegral)
