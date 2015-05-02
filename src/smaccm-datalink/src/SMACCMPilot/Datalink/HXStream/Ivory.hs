{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Hx streaming library.

module SMACCMPilot.Datalink.HXStream.Ivory where

import Ivory.Language
import Ivory.Stdlib
import SMACCMPilot.Datalink.HXStream.Ivory.Types

--------------------------------------------------------------------------------

type Hx  = Struct "hxstream_state"
type Tag = Uint8

[ivory|

struct hxstream_state
  { offset  :: Stored Sint32
  ; fstate  :: Stored HXState
  ; ftag    :: Stored Uint8 -- Frame tag
  ; tagSeen :: Stored IBool  -- Has a tag been processed in the current frame?
  }

|]

--------------------------------------------------------------------------------

emptyStreamState :: Ref s Hx -> Ivory eff ()
emptyStreamState state = do
  store (state ~> offset)  0
  store (state ~> fstate)  hxstate_progress
  store (state ~> tagSeen) false
  -- Don't care about the frame tag---shouldn't be read unless there's a new
  -- frame.

-- Should match the initialization in the empty state.
initStreamState :: Init (Struct "hxstream_state")
initStreamState = istruct [ fstate  .=  ival hxstate_progress
                          , offset  .=  ival 0
                          , tagSeen .= ival false
                          ]

--------------------------------------------------------------------------------
-- State-setting helpers

setState :: Ref s Hx -> HXState -> Ivory eff ()
setState state = store (state ~> fstate)

setTag :: Ref s Hx -> Tag -> Ivory eff ()
setTag state t = do
  store (state ~> ftag) t
  store (state ~> tagSeen) true

-- Increment the offset by one.
tickOffset :: Ref s Hx -> Ivory eff ()
tickOffset state = do
  off <- state ~>* offset
  store (state ~> offset) (off+1)

--------------------------------------------------------------------------------
-- Hxstream bytes

fbo :: Uint8
fbo  = 0x7e

ceo :: Uint8
ceo  = 0x7c

--------------------------------------------------------------------------------

escape :: Uint8 -> Uint8
escape = (.^) 0x20 -- XOR with 0x20

--------------------------------------------------------------------------------

-- | Decode a byte, given an hxstream state.  Returns a byte.  When the function
-- returns, if the hxstate makes the following transitions, we have the
-- following bytes returned:
--
-- hxstate_tag      --> hxstate_progress : tag byte
-- hxstate_progress --> hxstate_progress : frame byte
-- hxstate_esc      --> hxstate_progress : escaped frame byte
--
-- If we have
--
-- hxstate_progress --> hxstate_tag
--
-- A frame just ended.
decodeSM :: Def ('[ Ref s Hx, Uint8 ] :-> Uint8)
decodeSM = proc "decodeSM" $ \state b -> body $ do
  st <- state ~>* fstate
  byteRef <- local (ival 0)
  cond_
    [   -- If you see fbo, we're starting a new frame.
        b ==? fbo
    ==> emptyStreamState state >> setState state hxstate_tag
        -- Get the tag in the tag state and get ready to process the rest.
    ,   st ==? hxstate_tag
    ==> setTag state b >> setState state hxstate_progress
        -- Progress
    ,   st ==? hxstate_progress
    ==> progress state b byteRef
        -- Handle escaped bytes.
    ,   st ==? hxstate_esc
    ==> do setState state hxstate_progress
           tickOffset state
           store byteRef (escape b)
        -- The impossible happened.
    ,   true
    ==> return ()
    ]
  ret =<< deref byteRef
  where
  progress state b byteRef =
    cond_ [ b ==? ceo ==> setState state hxstate_esc
          , true      ==> tickOffset state >> store byteRef b
          ]

--------------------------------------------------------------------------------

newtype FrameHandler =
  FrameHandler { unFrameHandler :: forall s. ScopedFrameHandler s }

mkFrameHandler :: (forall s. ScopedFrameHandler s) -> FrameHandler
mkFrameHandler = FrameHandler

data ScopedFrameHandler s =
  ScopedFrameHandler
    { -- ^ Tag to match.  Only matching frames with a matching tag will be
      -- handled.
      fhTag   :: Tag
      -- ^ What to do before parsing the frame, after matching the tag.
    , fhBegin :: Ivory (AllocEffects s) ()
      -- ^ What to do with a byte of frame data.  Can use the index.
    , fhData  :: Uint8 -> Sint32 -> Ivory (AllocEffects s) ()
      -- ^ What to do at the end of the frame.
    , fhEnd   :: Ivory (AllocEffects s) ()
    }

-- | Decode a byte given a list of frame handlers.
decodes :: forall s0 s1 . [FrameHandler]
        -> Ref s1 Hx
        -> Uint8
        -> Ivory (AllocEffects s0) ()
decodes fhs state b = do
  -- State before decoding byte.
  st0  <- state ~>* fstate
  off  <- state ~>* offset
  tagB <- state ~>* tagSeen

  byte <- call decodeSM state b

  -- State after decoding byte.
  st1  <- state ~>* fstate
  tag  <- state ~>* ftag

  -- Run each framehandler for which the tag matches.
  let go k (FrameHandler fh) = when (tag ==? fhTag fh) (k fh)
  let fhLookup :: (ScopedFrameHandler s0 -> Ivory (AllocEffects s0) ())
               -> Ivory (AllocEffects s0) ()
      fhLookup k = mapM_ (go k) fhs

  cond_
    [   -- Frame ended and we processed a full frame, including the tag.
        (st0 ==? hxstate_progress) .&& (st1 ==? hxstate_tag) .&& tagB
    ==> fhLookup fhEnd
        -- Getting tag: run beginning action for frame.
    ,   (st0 ==? hxstate_tag)
    ==> fhLookup fhBegin
        -- Got a frame byte: process it.
    ,   (st0 ==? hxstate_progress) .|| (st0 ==? hxstate_esc)
    ==> fhLookup (\fh -> fhData fh byte off)
        -- Idle otherwise.
    ,   true
    ==> return ()
   ]

-- | Decode a frame given a frame state and byte and a set of framehandler
-- functions.
decode :: FrameHandler
       -> Ref s1 Hx
       -> Uint8
       -> Ivory (AllocEffects s0) ()
decode fh = decodes [fh]

--------------------------------------------------------------------------------

-- | Takes a tag, frame array, and a 'put' function and encodes according to the
-- hxstream protocol.
encode ::   ANat n
         => Tag
         -> ConstRef s (Array n (Stored Uint8))
         -> (Uint8 -> Ivory ('Effects r NoBreak a) ())
         -> Ivory ('Effects r b a) ()
encode tag arr put = noBreak $ do
  put fbo
  put tag
  putencoded
  put fbo
  where
  putencoded = arrayMap $ \ix -> noBreak $ do
    v  <- deref (arr ! ix)
    ifte_ ((v ==? fbo) .|| (v ==? ceo))
          (put ceo >> put (escape v))
          (put v)

encodeString :: (ANat n, IvoryString str)
             => Tag
             -> ConstRef s1 (Array n (Stored Uint8))
             -> Ref s2 str
             -> Ivory ('Effects r b a) ()
encodeString tag arr str = do
  total <- deref $ str ~> stringLengthL
  let strlen = arrayLen (str ~> stringDataL)
  assert $ arrayLen arr * 2 + 3 <=? strlen .|| total * 2 + 3 <=? strlen
  encode tag arr $ \ ch -> do
    queued <- deref $ str ~> stringLengthL
    assert $ queued <? arrayLen (str ~> stringDataL)
    store (str ~> stringDataL ! toIx queued) ch
    store (str ~> stringLengthL) (queued + 1)

--------------------------------------------------------------------------------

hxstreamModule :: Module
hxstreamModule = package "hxstream_state_module" $ do
  defStruct (Proxy :: Proxy "hxstream_state")
  incl decodeSM

--------------------------------------------------------------------------------
