-- Mavlink (1.0) parser.
module Mavlink.Parser
  ( ParseSt(..)
  , emptyParseSt
  , parseStream
  , ProcessSt
  ) where

import qualified Data.ByteString as B
import Data.Word

-- Important!  Magic CRCs that are target-specific.
import SMACCMPilot.Mavlink.Messages (messageLensCRCs)

import GCS.Mavlink.CRC

--------------------------------------------------------------------------------

data ParseSt =
  ParseSt
    { gotMagic    :: Bool
    , decodedLen  :: Word8
    , packetType  :: Word8
    , packetOffs  :: Word8
    , crc         :: Word16
    , done        :: Bool -- Parse of packet complete?
    , maybePacket :: [Word8] -- A partially-parsed packet, which may fail.
    } deriving (Show, Read, Eq)

emptyParseSt :: ParseSt
emptyParseSt = ParseSt False 0 0 0 0 False []

type Error   = String

data ErrSt   = ErrSt { msg :: Error  -- ^ Error mesage
                     , drp :: Int    -- ^ Number of bytes to drop until we are
                                     -- done with the current corrupted packet.
                     } deriving (Show, Read, Eq)

type Packets   = [B.ByteString] -- Parsed Mavlink packets
type Result    = Either ErrSt ParseSt
type ProcessSt = ([Error], Packets, ParseSt)

--------------------------------------------------------------------------------

-- Parse a bytestream for Mavlink packets.
parseStream :: Word8 -- ^ Maximum number of bytes we'll parse before failing.
            -> ParseSt -- ^ Parser state.
            -> B.ByteString -- ^ Input.
            -> ProcessSt
parseStream maxBytes s = parseStream' ([], [], s)
  where
  parseStream' :: ProcessSt
               -> B.ByteString
               -> ProcessSt
  parseStream' (errs, packets, ps) bs
    | B.null bs
    = (errs, packets, ps)
    | otherwise
    = let b  = B.head bs in
      let t  = B.tail bs in
      case parseByte ps b of
        -- If there's an error, reset the parser and skip the designated number
        -- of bytes.  -- XXX fix: might not drop enough bytes.
        Left err -> parseStream'
                      (msg err : errs, packets, emptyParseSt)
                      (B.drop (drp err) bs)
        Right st | packetOffs st > maxBytes
                 -> parseStream'
                      ("Packet too long!" : errs, packets, emptyParseSt)
                      t
                 | done st
                 -> parseStream'
                      (errs, packets ++ [B.pack $ maybePacket st], emptyParseSt)
                      t
                 | otherwise
                 -> parseStream' (errs, packets, st) t

-- Returns a possible error message and a new parse state.
parseByte :: ParseSt -> Word8 -> Result
parseByte s b
  -- Saw the magic start byte.  Process packet.
  | gotMagic s =
      case packetOffs s of
        -- Get the length.
        1 -> next b s { decodedLen = b }
        2 -> next b s -- Sequence number
        3 -> next b s -- System ID
        4 -> next b s -- Comp ID
        -- 6th byte is the message ID
        5 -> do s' <- validatePacketLen s { packetType = b }
                next b s'
        off -> case off - decodedLen s of
                 6 -> do s' <- withCRCExtra s
                         let s'' = nextNoCRC b s'
                         if b == crc_lo s''
                           then ok s''
                           else crcErr ("invalid crc_lo: expected "
                                        ++ show (crc_lo s''))
                                       1
                 7 -> if b == crc_hi s
                         -- Successfully parsed a packet.
                         then ok $ s { done = True
                                     , maybePacket = reverse (b : maybePacket s)
                                     }
                         else crcErr ("invalid crc_hi: expected "
                                      ++ show (crc_hi s))
                                     0
                 -- offset should be negative or < 6 here.  Get the payload.
                 _ -> next b s
                 where
                 crcErr str n = Left (ErrSt str n)
                 crc_lo ss    = fst (crc_lo_hi (crc ss))
                 crc_hi ss    = snd (crc_lo_hi (crc ss))
  | otherwise =
      case b of
        254 -> ok $ addByte b $ s { gotMagic = True
                                  , crc = 0xFFFF
                                  , packetOffs = 1
                                  }
        _   -> ok s -- Don't add byte here---not parsing a packet.

ok :: ParseSt -> Result
ok = Right

next :: Word8 -> ParseSt -> Result
next b s = ok $ nextNoCRC b s { crc = crc_accumulate (crc s) b }

addByte :: Word8 -> ParseSt -> ParseSt
addByte b s = s { maybePacket = b : maybePacket s }

nextNoCRC :: Word8 -> ParseSt -> ParseSt
nextNoCRC b s = addByte b
              $ s { packetOffs = 1 + packetOffs s }

-- If we can't validate the packet, we'll skip the packetType payload length (+
-- 2 bytes for the CRC) if it exists and the reported length otherwise.
validatePacketLen :: ParseSt -> Result
validatePacketLen s =
  case lookup (packetType s) messageLensCRCs of
    Just (len, _) | len == decLen -> Right s
                  | otherwise     -> Left (ErrSt errmsg $ plustwo len)
    _                             -> Left (ErrSt errmsg $ plustwo decLen)
  where
  errmsg    = "invalid packet len: expected " ++ show decLen
  decLen    = decodedLen s
  plustwo l = fromIntegral l + 2

withCRCExtra :: ParseSt -> Result
withCRCExtra s =
  case lookup (packetType s) messageLensCRCs of
    Just (_, extra) -> Right s { crc = crc_accumulate (crc s) extra }
    _               -> Left (ErrSt errmsg 1)
  where
  errmsg = "Can't find crc extra of validated packet, should be impossible"
