{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ < 709
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Haskell implementation of the Hxstream protocol.

module SMACCMPilot.Datalink.HXStream.Native
  ( FrameState(..)
  , StreamState(..)
  , fbo
  , ceo
  , emptyStreamState
  , extractFrame
  , decodeSM
  , decodeByte
  , decode
  , encode
  , Tag
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Word
import qualified Data.DList as D

--------------------------------------------------------------------------------

type Tag = Word8

instance {-# OVERLAPPING #-} Show (D.DList Word8) where
  show = show . D.toList

data FrameState = FrameTag
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
emptyStreamState = StreamState D.empty Nothing FrameProgress

-- | Get a `ByteString` frame from a frame state.
extractFrame :: StreamState -> B.ByteString
extractFrame = B.pack . D.toList . frame

-- Append a byte to the frame, escaping if needed.  Assumes we're in the
-- progress state.
appendFrame :: Word8 -> StreamState -> StreamState
appendFrame b s = s { frame = frame s .++ b }

-- | Frame Boundary Octet
fbo :: Word8
fbo  = 0x7e
-- | Control Escape Octet
ceo :: Word8
ceo  = 0x7c

-- | Escape transformation
escape :: Word8 -> Word8
escape b = b `xor` 0x20

-- | When fstate state == FrameTag, the input state contains a completed frame.
-- The new state is reset.
decodeSM :: Word8 -> StreamState -> StreamState
decodeSM b state
  -- If you see fbo, we're starting a new frame.
  | b == fbo
  = emptyStreamState { fstate = FrameTag }
  -- Process the tag byte.
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
    case fstate st' of
      FrameTag -> case ftag st of
                    -- No tag, throw the frame away.
                    Nothing -> (fs, st')
                    Just t  -> (fs .++ (t, extractFrame st), st')
      _        -> (fs, st')
    where
    st' = decodeSM b st

-- | Decode an hxstream.  Returns a list of decoded frames and their tags.
decodeByte :: Word8
           -> StreamState
           -> (Maybe (Tag, B.ByteString), StreamState)
decodeByte b state = (r, st')
  where
  st' = decodeSM b state
  r = case fstate st' of
      FrameTag ->
        case ftag state of
           Just t  -> Just (t, extractFrame state)
           Nothing -> Nothing
      _ -> Nothing

encode :: Tag -> B.ByteString -> B.ByteString
encode tag ws' = B.pack $ D.toList $ fbo .: tag .: go ws
  where
  ws = B.unpack ws'
  go (x:xs) | x == fbo  = ceo .: (escape x .: go xs)
            | x == ceo  = ceo .: (escape x .: go xs)
            | otherwise = x .: go xs
  go [] = D.singleton fbo

-- Helpers

(.:) :: a -> D.DList a -> D.DList a
(.:) = D.cons
infixr .:

(.++) :: D.DList a -> a -> D.DList a
(.++) = D.snoc
