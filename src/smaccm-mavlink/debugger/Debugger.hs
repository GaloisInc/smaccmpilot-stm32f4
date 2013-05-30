
module Debugger where

import qualified Data.Char as C
import Data.List (lookup)
import Numeric (showHex)
import Control.Monad

import qualified Data.ByteString as B
import Data.Word
import System.Hardware.Serialport

import SMACCMPilot.Mavlink.Messages (messageLensCRCs)

import CRC

data DebuggerState =
  DebuggerState
    { gotMagic :: Bool
    , decodedLen :: Word8
    , packetType :: Word8
    , packetOffs :: Word8
    , crc :: Word16
    }

emptyDebuggerState = DebuggerState False 0 0 0 0

debuggerLoop :: SerialPort -> DebuggerState -> IO DebuggerState
debuggerLoop p state = do
  bs <- recv p 1
  foldM processByte state (B.unpack bs)

processByte :: DebuggerState -> Word8 -> IO DebuggerState
processByte s b = do
  putStr ("0x" ++ (showHex b " "))
  case gotMagic s of
    True -> do
      case packetOffs s of
        1 -> do putStrLn ("> len " ++ show b)
                return $ next b $ s { decodedLen = b}
        2 -> putStrLn "> seqnum" >> return (next b s)
        3 -> putStrLn "> sysid"  >> return (next b s)
        4 -> putStrLn "> compid"  >> return (next b s)
        5 -> do putStrLn ("> ptype " ++ show b)
                s' <- validatePacketLen $ s { packetType = b }
                return $ next b $ s'
        off -> case off - (decodedLen s) of
                 6 -> let s' = withCRCExtra s in
                      putStrLn ("> crc1 " ++ (valid (crc_lo s') b)) >>
                      return (nextNoCRC s')
                 7 -> putStrLn ("> crc2 " ++ (valid (crc_hi s) b)) >>
                      return emptyDebuggerState
                 _ -> putStrLn ("> pay[" ++ (show (off - 6))++ "]") >> return (next b s)
                 where
                 crc_lo ss = fst (crc_lo_hi (crc ss))
                 crc_hi ss = snd (crc_lo_hi (crc ss))
                 valid expected got =
                   if expected == got then "ok" else "bad, expected 0x" ++ (showHex got "")

    False -> do
      case b of
        254 -> do
          putStrLn "> start"
          return $ s { gotMagic = True, crc = 0xFFFF, packetOffs = 1 }
        _ -> do
          putStrLn "> skip"
          return s

next :: Word8 -> DebuggerState -> DebuggerState
next b s = nextNoCRC $ s { crc = (crc_accumulate (crc s) b)}

nextNoCRC :: DebuggerState -> DebuggerState
nextNoCRC s = s { packetOffs = 1 + (packetOffs s) }

validatePacketLen :: DebuggerState -> IO DebuggerState
validatePacketLen s =
  case lookup (packetType s) messageLensCRCs of
    Just (len, _) | len == (decodedLen s) -> approve
    _ ->  fail
  where
  approve = return s
  fail = putStrLn "(len fail)" >> return emptyDebuggerState

withCRCExtra :: DebuggerState -> DebuggerState
withCRCExtra s =
  case lookup (packetType s) messageLensCRCs of
    Just (_, extra) -> s { crc = (crc_accumulate (crc s) extra) }
    _ -> error "cant find crc extra of validated packet, should be impossible"

