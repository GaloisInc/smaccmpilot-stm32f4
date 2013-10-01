{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Hx streaming library.

module Ivory.HXStream where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HXStream.Types

import qualified SMACCMPilot.Shared as S

--------------------------------------------------------------------------------

type Hx  = Struct "hxstream_state"

-- XXX Can't used shared types in TH.  This will throw a type-error if other
-- types change, though.
 -- Raw bytes to encode/decoded.
--    buf     :: Array 128 (Stored Uint8)

[ivory|
struct hxstream_state
  { offset  :: Stored Sint32
  ; fstate  :: Stored HXState
  -- True if we couldn't encode since the input was too big
  ; ovf     :: Stored IBool
  -- Frame tag
  ; ftag    :: Stored Uint8
  }
|]

emptyStreamState :: Ref s Hx -> Ivory eff ()
emptyStreamState state = do
  -- arrayMap $ \ix -> store ((state ~> buf) ! ix) 0
  store (state ~> offset)  0
  store (state ~> fstate)  hxstate_fstart
  store (state ~> ovf) false
  -- Don't care about the frame tag---shouldn't be read unless there's a new
  -- frame.

setState :: Ref s Hx -> HXState -> Ivory eff ()
setState state = store (state ~> fstate)

-- Set the overflow (ovf) bit if we run out of buffer.
-- Otherwise, store the bytes.
appendFrame :: SingI n
            => Uint8
            -> Ref s0 Hx
            -> Ref s1 (Array n (Stored Uint8))
            -> Ivory eff ()
appendFrame b s buf = do
  ix <- deref (s ~> offset)
  ifte_ (ix >=? arrayLen buf)
        (store (s ~> ovf) true)
        (   store (buf ! toIx ix) b
         >> store (s ~> offset) (ix+1))

fbo :: Uint8
fbo  = 0x7e

ceo :: Uint8
ceo  = 0x7c

escape :: Uint8 -> Uint8
escape = (.^) 0x20 -- XOR with 0x20

-- | Decode a byte, given an hxstream state.  Updates the hxstream state with
-- the decoded value.  Returns true if we've decoded a full frame.  Safe to have
-- a start byte follow a stop byte directly.  The caller is responsible for
-- moving/copying the decoded array out once `True` is returned.
decodeSM :: SingI n
         => Def ('[ Ref s0 Hx
                  , Ref s1 (Array n (Stored Uint8))
                  , Uint8] :-> IBool)
decodeSM = proc "decodeSM" $ \state buf b -> body $ do
  st <- state ~>* fstate
  cond_
    -- Begin state and see a start byte.
    [   (st ==? hxstate_fstart) .&& (b ==? fbo)
    ==> emptyStreamState state >> setState state hxstate_tag
    ,   st ==? hxstate_tag
    ==> store (state ~> ftag) b >> setState state hxstate_data
    -- Progress state: decode the byte.
    ,   st ==? hxstate_data
    ==> progress state buf b
    -- Escape state
    ,   st ==? hxstate_esc
    ==> appendFrame (escape b) state buf >> setState state hxstate_data
    -- Stop state: reset the state and process the curent byte.  This ensures we
    -- don't miss a byte if there's a start byte directly following a stop byte.
    ,   st ==? hxstate_idle
    ==> do emptyStreamState state -- rests state to hxstate_fstart
           ret =<< call decodeSM state buf b
    -- Begin state and no start byte: idle.
    ,   true
    ==> return ()
    ]

  s <- state ~>* fstate
  ret (s ==? hxstate_idle)
  where
  progress state buf b =
    cond_ [ b ==? ceo ==> setState state hxstate_esc
          , b ==? fbo ==> setState state hxstate_idle
          , true      ==> appendFrame b state buf
          ]

-- Fixed for commsec array length
decodeSMCommsec :: Def ('[ Ref s0 Hx
                         , Ref s1 S.CommsecArray
                         , Uint8] :-> IBool)
decodeSMCommsec = decodeSM

-- | Encode a commsec array byte array into an hxstream byte array.  This
-- guarantees we have enough storage to hold the input array.
encode :: Def ( '[ Uint8 -- ^ Message tag
                 , Ref s0 S.CommsecArray -- ^ From array
                 , Ref s1 S.HxstreamArray -- ^ To array
                 ]
                 :-> ())
encode = proc "encode" $ \tag from to -> body $ do
  off <- local (ival (0 :: S.HxstreamIx))
  -- start byte
  store (to ! 0) fbo
  -- store the tag (without escaping)
  storeTo to off tag
  -- encode the rest
  arrayMap $ \ix -> do
    v  <- deref (from ! ix)
    cond_ [   -- Handle escape chars.
              (v ==? fbo) .|| (v ==? ceo)
          ==> do storeTo to off ceo
                 storeTo to off (escape v)
              -- Just add the byte.
          ,   true
          ==> storeTo to off v
          ]
  retVoid

  where
  -- increment then use it as an index.
  storeTo to off v = do
    off %= (+1)
    o <- deref off
    store (to ! o) v

hxstreamModule :: Module
hxstreamModule = package "hxstream_state_module" $ do
  defStruct (Proxy :: Proxy "hxstream_state")
  incl encode
  incl decodeSMCommsec
