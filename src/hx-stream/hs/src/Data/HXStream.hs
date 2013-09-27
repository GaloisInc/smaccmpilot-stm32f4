module Data.HXStream where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Word
import qualified Data.DList as D
import           Data.List

import qualified SMACCMPilot.Shared as S

type Frame = [Word8]

data FrameState = FrameBegin
                | FrameProgress Bool -- indicates escaped mode
                | FrameComplete
                deriving (Eq, Show)

data StreamState =
  StreamState
    { frame   :: D.DList Word8
    , fstate  :: FrameState
    }

instance Show StreamState where
  show (StreamState f st) =
    "StreamState { " ++ show (D.toList f) ++ ", " ++ show st ++ " }"

emptyStreamState :: StreamState
emptyStreamState = StreamState D.empty FrameBegin

appendFrame :: Word8 -> StreamState -> StreamState
appendFrame b s = s { frame = frame s .++ b }

setEscaped :: Bool -> StreamState -> StreamState
setEscaped b s = s { fstate = FrameProgress b }

complete :: StreamState -> Bool
complete s =
  case fstate s of
    FrameComplete -> True
    _             -> False

-- If we've completed a frame, return it.
completeFrame :: StreamState -> Maybe Frame
completeFrame s = case complete s of
  True  -> Just (D.toList $ frame s)
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
    FrameProgress escaped
      | escaped   -> setEscaped False $ appendFrame (escape b) state
      | b == ceo  -> setEscaped True state
      | b == fbo  -> state { fstate = FrameComplete }
      | otherwise -> appendFrame b state
    FrameComplete -> state

-- | Decode an hxstream.  Returns a list of decoded frames of no more than
-- commsecPkgSize bytes and a state (which may contain an incompletely-decoded
-- frame).
decode :: B.ByteString -> StreamState -> ([B.ByteString], StreamState)
decode bs istate = (frames, newSt)
  where
  frames        = map (B.take $ fromInteger S.commsecPkgSize) byteFrames
  byteFrames    = map B.pack (D.toList fr)
  (fr, newSt)   = foldl' aux (D.empty, istate) (B.unpack bs)
  aux (fs,st) w =
    case completeFrame s' of
      Just f  -> (fs .++ f, emptyStreamState)
      Nothing -> (fs, s')
    where
    s' = decodeSM w st

encode :: Frame -> B.ByteString
encode ws = B.pack $ D.toList $ fbo .: (go ws .++ fbo)
  where
  go (x:xs) | x == fbo  = ceo .: (escape x .: go xs)
            | x == ceo  = ceo .: (escape x .: go xs)
            | otherwise = x .: go xs
  go [] = D.empty

-- Helpers

(.:) :: a -> D.DList a -> D.DList a
(.:) = D.cons

(.++) :: D.DList a -> a -> D.DList a
(.++) = D.snoc
