
-- Mavlink (1.0) parser.
module SMACCMPilot.Mavlink.Native.Parse where

import Control.Monad

import Data.Word

-- Important!  Magic CRCs that are target-specific.
import SMACCMPilot.Mavlink.Native.Messages (messageLensCRCs)
import SMACCMPilot.Mavlink.Native.CRC

data Verboseness = Chatty | Quiet deriving (Eq, Show)

data DebuggerState =
  DebuggerState
    { gotMagic   :: Bool
    , decodedLen :: Word8
    , packetType :: Word8
    , packetOffs :: Word8
    , crc        :: Word16
    }

data Result =
  Result
    { res_v :: Word8
    , res_t :: Tag
    } deriving (Eq, Show)

-- Just for debugging.
data Tag = Start
         | Len
         | SeqNum
         | SysId
         | CompId
         | PType
         | Payload Word8
         | CRC1
         | CRC2
         | Fail String
         | Skip
         deriving (Eq, Show)

emptyDebuggerState :: DebuggerState
emptyDebuggerState = DebuggerState False 0 0 0 0

debuggerLoop :: [Word8]
             -> (DebuggerState,[Result])
             -> Verboseness
             -> IO (DebuggerState,[Result])
debuggerLoop bs state v = foldM (processByte v) state bs

printProcessed :: Verboseness -> [Result] -> IO ()
printProcessed Chatty rs  = mapM_ (\r -> putStrLn (show r)) rs
printProcessed Quiet rs   = putStrLn (unlines (mkQuietMsgs rs))

processByte :: Verboseness
            -> (DebuggerState,[Result])
            -> Word8
            -> IO (DebuggerState,[Result])
processByte v (s,results) b = do
  let (res, s') = parseByte s b
      res'      = res:results
      prt       = printProcessed v (reverse res') >> return (s', [])
      continue  = return (s', res:results)
  case res_t res of
    Skip   -> prt
    Fail _ -> prt
    CRC2   -> prt
    _      -> continue

parseByte :: DebuggerState -> Word8 -> (Result, DebuggerState)
parseByte s b =
  case gotMagic s of
    True ->
      case packetOffs s of
        1 -> res Len $ next b $ s { decodedLen = b}
        2 -> res SeqNum (next b s)
        3 -> res SysId  (next b s)
        4 -> res CompId (next b s)
        5 -> let (t, s') = validatePacketLen $ s { packetType = b } in
             res t $ next b $ s'
        off -> case off - (decodedLen s) of
                 6 -> let s' = withCRCExtra s
                          r t = res t (nextNoCRC s')
                      in if b == (crc_lo s')
                         then r CRC1
                         else r $ Fail $ "invalid crc_lo: expected "
                                         ++ show (crc_lo s')
                 7 -> if b == (crc_hi s)
                         then res CRC2 emptyDebuggerState
                         else res (Fail $ "invalid crc_hi: expected "
                                          ++ show (crc_hi s))
                                  emptyDebuggerState
                 _ -> res (Payload (off - 6)) (next b s)
                 where
                 crc_lo ss = fst (crc_lo_hi (crc ss))
                 crc_hi ss = snd (crc_lo_hi (crc ss))

    False -> do
      case b of
        254 -> res Start $ s { gotMagic = True, crc = 0xFFFF, packetOffs = 1 }
        _   -> res Skip s
  where
  res :: Tag -> DebuggerState -> (Result, DebuggerState)
  res t ds = (Result b t, ds)

next :: Word8 -> DebuggerState -> DebuggerState
next b s = nextNoCRC $ s { crc = (crc_accumulate (crc s) b)}

nextNoCRC :: DebuggerState -> DebuggerState
nextNoCRC s = s { packetOffs = 1 + (packetOffs s) }

validatePacketLen :: DebuggerState -> (Tag, DebuggerState)
validatePacketLen s =
  case lookup (packetType s) messageLensCRCs of
    Just (len, _) | len == (decodedLen s) -> (PType, s)
    _ ->  (Fail errmsg , emptyDebuggerState)
  where errmsg = "invalid packet len: expected " ++ (show (decodedLen s))

withCRCExtra :: DebuggerState -> DebuggerState
withCRCExtra s =
  case lookup (packetType s) messageLensCRCs of
    Just (_, extra) -> s { crc = (crc_accumulate (crc s) extra) }
    _ -> error "cant find crc extra of validated packet, should be impossible"

mkQuietMsgs :: [Result] -> [String]
mkQuietMsgs = const [] -- XXX

