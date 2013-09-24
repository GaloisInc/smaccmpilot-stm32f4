{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.HXStream
  ( decodeSM
  , ScopedFrameHandler(..)
  , FrameHandler(..)
  , encodeToBS
  , Hx
  , hx_init
  , hx_ival
  , hxstream_types
  ) where

import Ivory.Language
import Ivory.Stdlib

import Ivory.HXStream.Types
import Ivory.HXStream.Struct
--------------------------------------------------------------------------------

hx_ival :: Init Hx
hx_ival = istruct [ state .= ival hxstate_idle ]

hx_init :: Ref s Hx -> Ivory eff ()
hx_init hx = do
  store (hx ~> state)   hxstate_idle

fbo :: Uint8
fbo  = 0x7e

ceo :: Uint8
ceo  = 0x7c

escape :: Uint8 -> Uint8
escape b = b .^ 0x20 -- XOR with 0x20

newtype FrameHandler = FrameHandler { unFrameHandler :: forall cs . ScopedFrameHandler cs }

data ScopedFrameHandler cs =
  ScopedFrameHandler
    { fh_tag   :: Uint8
    , fh_begin :: Ivory (AllocEffects cs) ()
    , fh_data  :: Uint8 -> Sint32 -> Ivory (AllocEffects cs) ()
    , fh_end   :: Ivory (AllocEffects cs) ()
    }

-- | Decode a byte, given an hxstream state.  Updates the hxstream state with
-- the decoded value. Gives decoded info to frame handlers.
decodeSM :: forall s cs
          . [FrameHandler]
         -> Ref s Hx
         -> Uint8
         -> Ivory (AllocEffects cs) ()
decodeSM handls hx c = do
  s <- deref (hx ~> state)
  cond_
    [ (s ==? hxstate_idle) .&& (c ==? fbo)
      ==> store (hx ~> state) hxstate_fstart
    , (s ==? hxstate_fstart)  ==> taghandler
    , (s ==? hxstate_data)    ==> datahandler
    , (s ==? hxstate_esc)     ==> eschandler
    ]
  where
  hs :: [ScopedFrameHandler cs]
  hs = map unFrameHandler handls
  taghandler = cond_ ((fborestart : (map tagdetect hs)) ++
    [true ==> store (hx ~> state) hxstate_idle])
  fborestart = ((c ==? fbo) ==> store (hx ~> state) hxstate_fstart)
  tagdetect fh = (c ==? (fh_tag fh)) ==> do
    store (hx ~> state)  hxstate_data
    store (hx ~> offset) 0
    store (hx ~> ftag)   (fh_tag fh)
    fh_begin fh
  endcheck tag = c ==? fbo ==> do
    store (hx ~> state) hxstate_idle
    let handleend fh = tag ==? (fh_tag fh) ==> fh_end fh
    cond_ (map handleend hs)
  incroffs = do
     offs <- deref (hx ~> offset)
     store (hx ~> offset) (offs + 1)
     return offs
  handlebyte tag cc offs = cond_ (map h hs)
    where h fh = tag ==? (fh_tag fh) ==> fh_data fh cc offs
  datahandler = do
    tag <- deref (hx ~> ftag)
    let esccheck = c ==? ceo ==>
          store (hx ~> state) hxstate_esc
        dispatchdata = do
          offs <- incroffs
          handlebyte tag c offs
    cond_ [esccheck, endcheck tag, true ==> dispatchdata]
  eschandler = do
    tag <- deref (hx ~> ftag)
    let dispatchescaped = do
          offs <- incroffs
          handlebyte tag (escape c) offs
    cond_ [endcheck tag, true ==> dispatchescaped]
    store (hx ~> state) hxstate_data

-- Encode from a framed array to a byte stream.
encodeToBS :: (SingI n)
         => Uint8
         -> ConstRef s (Array n (Stored Uint8))
         -> (Uint8 -> Ivory (AllocEffects cs) ())
         -> Ivory (AllocEffects cs) ()
encodeToBS tag arr put = do
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

