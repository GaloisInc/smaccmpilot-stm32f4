
module Data.HXStream where

import           Data.Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import           Data.Word

newtype Frame = Frame { unFrame :: [Word8] } deriving (Eq, Show)

data FrameState = FrameBegin
                | FrameProgress Bool -- indicates escaped mode
                | FrameComplete
                deriving (Eq, Show)

data StreamState =
  StreamState
    { frame   :: [Word8]
    , fstate  :: FrameState
    } deriving (Eq, Show)

emptyStreamState :: StreamState
emptyStreamState = StreamState [] FrameBegin

appendFrame :: Word8 -> StreamState -> StreamState
appendFrame b s = s { frame = (frame s) ++ [b] }

setEscaped :: Bool -> StreamState -> StreamState
setEscaped b s = s { fstate = FrameProgress b }

complete :: StreamState -> Bool
complete s = case fstate s of
  FrameComplete -> True
  _ -> False

completeFrame :: StreamState -> Maybe Frame
completeFrame s = case complete s of
  True  -> Just (Frame (frame s))
  False -> Nothing

-- | Frame Boundary Octet
fbo :: Word8
fbo  = 0x7e

-- | Control Escape Octet
ceo :: Word8
ceo  = 0x7c

-- | Escape transformation
escape :: Word8 -> Word8
escape b = b `xor` 0x20

decodeSM :: Word8 -> StreamState -> StreamState
decodeSM b state =
  case fstate state of
    FrameBegin ->
      if b == fbo then setEscaped False $ emptyStreamState
      else state
    FrameProgress escaped ->
      if escaped        then setEscaped False $ appendFrame (escape b) state
      else if b == ceo  then setEscaped True state
      else if b == fbo  then state { fstate = FrameComplete }
      else appendFrame b state
    FrameComplete -> state

decode :: B.ByteString -> StreamState -> ([Frame],StreamState)
decode bs istate = foldl aux ([],istate) w8s
  where
  w8s :: [Word8]
  w8s = map (fromIntegral . C.ord) $ B.unpack bs
  aux (fs,st) w =
    case completeFrame s' of
      Just f -> (fs ++ [f], emptyStreamState)
      Nothing -> (fs, s')
    where
    s' = decodeSM w st

encode :: Frame -> B.ByteString
encode (Frame ws) = B.pack $ map (C.chr . fromIntegral) $
  [fbo] ++ go ws ++ [fbo]
  where
  go (x:xs) | x == fbo  = ceo:(escape x):(go xs)
            | x == ceo  = ceo:(escape x):(go xs)
            | otherwise = x:(go xs)
  go [] = []

