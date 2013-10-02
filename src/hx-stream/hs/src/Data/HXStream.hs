{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Haskell implementation of the Hxstream protocol.

module Data.HXStream
  ( FrameState(..)
  , StreamState(..)
  , fbo
  , ceo
  , emptyStreamState
  , completedFrame
  , decodeSM
  , decode
  , encode
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Word
import qualified Data.DList as D
import           Data.Maybe

--------------------------------------------------------------------------------

type Tag = Word8

instance Show (D.DList Word8) where
  show = show . D.toList

data FrameState = FrameComplete
                | FrameTag
                | FrameProgress
                | FrameEscape
                deriving (Eq, Show)

data StreamState =
  StreamState
    { frame   :: D.DList Word8  -- Current frame
    , ftag    :: Maybe Word8
    , fstate  :: FrameState
    } deriving Show

emptyStreamState :: StreamState
emptyStreamState = StreamState D.empty Nothing FrameComplete

-- Append a byte to the frame, escaping if needed.  Assumes we're in the
-- progress state.
appendFrame :: Word8 -> StreamState -> StreamState
appendFrame b s = s { frame = frame s .++ b }

frameProgress :: StreamState -> Bool
frameProgress st = case fstate st of
                     FrameProgress -> True
                     FrameEscape   -> True
                     _             -> False

-- If we've completed a frame, return it.
completedFrame :: StreamState -> Maybe (Tag, B.ByteString)
completedFrame (StreamState fr tag st) =
  case st == FrameComplete && isJust tag of
    True  -> Just (fromJust tag, B.pack $ D.toList fr)
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

-- | When fstate state == FrameComplete && there's a tag, we have a completed
-- frame to pull off.
decodeSM :: Word8 -> StreamState -> StreamState
decodeSM b state
  -- If you see fbo and but we're not in a complete state, it's an end byte.
  | b == fbo && fstate state /= FrameComplete
  = state { fstate = FrameComplete }
  -- Otherwise an fbo is a start byte, so we expect a tag next.  Throw away any
  -- legacy state.
  | b == fbo && fstate state == FrameComplete
  = emptyStreamState { fstate = FrameTag }
  -- Get the tag in the tag state and get ready to process the rest.
  | fstate state == FrameTag
  = state { ftag = Just b, fstate = FrameProgress }
  -- Progress
  | fstate state == FrameProgress
  = progress
  -- Handle escpaed bytes.
  | fstate state == FrameEscape
  = appendFrame (escape b) state { fstate = FrameProgress }
  -- The impossible happened.  Reset.
  | otherwise
  = emptyStreamState
  where
  progress
    -- If you see ceo, set escape.
    | b == ceo
    = state { fstate = FrameEscape }
    -- Otherwise, append bytes to our current frame.
    | otherwise
    = appendFrame b state

-- | Decode an hxstream.  Returns a list of decoded frames and their tags.
decode :: B.ByteString
       -> StreamState
       -> ([(Tag, B.ByteString)], StreamState)
decode bs state = (D.toList dframes, newSt)
  where
  (dframes, newSt) = B.foldl' aux (D.empty, state) bs
  aux (fs,st) b =
    case completedFrame s' of
      Just tagfr -> (fs .++ tagfr, emptyStreamState)
      Nothing    -> (fs, s')
    where
    s' = decodeSM b st

encode :: Tag -> B.ByteString -> B.ByteString
encode tag ws' = B.pack $ D.toList $ fbo .: tag .: go ws
  where
  ws = B.unpack ws'
  go (x:xs) | x == fbo  = ceo .: (escape x .: go xs)
            | x == ceo  = ceo .: (escape x .: go xs)
            | otherwise = x .: go xs
  go [] = D.empty

-- Helpers

(.:) :: a -> D.DList a -> D.DList a
(.:) = D.cons
infixr .:

(.++) :: D.DList a -> a -> D.DList a
(.++) = D.snoc
