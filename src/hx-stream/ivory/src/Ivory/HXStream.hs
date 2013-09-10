{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Hx stream.  We assume incoming data can be stored in 128 Uint8s, after
-- encoding.

module Ivory.HXStream where

import Ivory.Language
import Ivory.Stdlib

--------------------------------------------------------------------------------

hxstream_fstate_Begin    :: Uint8
hxstream_fstate_Begin    = 0
hxstream_fstate_Progress :: Uint8
hxstream_fstate_Progress = 1
hxstream_fstate_Complete :: Uint8
hxstream_fstate_Complete = 2

type Hx  = Struct "hxstream_state"

[ivory|
struct hxstream_state
  { buf     :: Array 128 (Stored Uint8) -- Raw bytes to encode/decoded.
  ; offset  :: Stored Sint32
  ; fstate  :: Stored Uint8
  ; escaped :: Stored IBool
  -- True if we couldn't encode since the input was too big
  ; ovf     :: Stored IBool
  }
|]

emptyStreamState :: Ref s Hx -> Ivory eff ()
emptyStreamState state = do
  store (state ~> offset)  0
  store (state ~> fstate)  hxstream_fstate_Begin
  store (state ~> escaped) false
  store (state ~> ovf) false

-- Set the overflow (ovf) bit if we run out of buffer.
appendFrame :: Uint8 -> Ref s Hx -> Ivory eff ()
appendFrame b s = do
  ix <- deref (s ~> offset)
  let arr = (s ~> buf)
  ifte_ (ix + 1 >=? arrayLen arr)
        (store (s ~> ovf) true)
        (   store (arr ! toIx ix) b
         >> store (s ~> offset) (ix+1))

setEscaped :: IBool -> Ref s Hx -> Ivory eff ()
setEscaped b s = do
  store (s ~> fstate) hxstream_fstate_Progress
  store (s ~> escaped) b

setComplete :: Ref s Hx -> Ivory eff ()
setComplete s = store (s ~> fstate) hxstream_fstate_Complete

complete :: Ref s Hx -> Ivory eff IBool
complete state = do
  sta <- deref (state ~> fstate)
  return (sta ==? hxstream_fstate_Complete)

fbo :: Uint8
fbo  = 0x7e

ceo :: Uint8
ceo  = 0x7c

escape :: Uint8 -> Uint8
escape b = b .^ 0x20 -- XOR with 0x20

-- | Decode a byte, given an hxstream state.  Updates the hxstream state with
-- the decoded value.  Returns true if we've decoded a full frame.
decodeSM :: Ref s Hx
         -> Uint8
         -> Ivory eff IBool
decodeSM state b = do
  sta <- deref (state ~> fstate)
  esc <- deref (state ~> escaped)
  cond_
    [   (sta ==? hxstream_fstate_Begin) .&& (b ==? fbo)
    ==> (emptyStreamState state >> setEscaped false state)
    ,   sta ==? hxstream_fstate_Progress
    ==> progress esc
    ,   sta ==? hxstream_fstate_Complete
    ==> return ()
    ,   true
    ==> emptyStreamState state
    ]
  s <- deref (state ~> fstate)
  return (s ==? hxstream_fstate_Complete)
    where
    progress esc =
      cond_ [ esc       ==> (   setEscaped false state
                             >> appendFrame (escape b) state)
            , b ==? ceo ==> setEscaped true state
            , b ==? fbo ==> setComplete state
            , true      ==> appendFrame b state
            ]

-- | Decode an hxstreamed array into the raw bytes.  The input array can be
-- arbitrary size.  Decoding stops if (1) the hxstream state we're decoding into
-- is full (2) a stop byte is encountered (the input array may contain hxstream
-- frame boundaries).  An index to the first byte not decoded is returned.  (If
-- the returned index is 0, and the hxstream state doesn't have the overflow bit
-- set, then the full array was fully decoded (assuming the input array has
-- length of 2 or greater)).
decode :: SingI n =>
       Def ( '[ Ref s (Array n (Stored Uint8))
              , Ref s Hx
              ] :-> Ix n)
decode = proc "decode" $ \from state -> body $ do
  arrayMap $ \ix -> do
    v    <- deref (from ! ix)
    b    <- decodeSM state v
    over <- deref (state ~> ovf)
    -- Couldn't put the current byte in---we overflowed.
    when over (ret ix)
    -- Won't put the next byte in---we're done.
    when b (ret (ix+1))
  -- Return index 0 if all went well.
  ret 0

hxstreamTypeModule :: Module
hxstreamTypeModule = package "hxstream_type" $ do
  defStruct (Proxy :: Proxy "hxstream_state")

-- | Encode from some portion of a 128 byte array, up to and including a stop
-- index, into a 258 byte array.  This guarantees we have enough storage to hold
-- the 128 bytes.  A pointer is returned into the "to" array pointing to the
-- last encoded element.
encode :: Def ( '[ Ref s (Array 128 (Stored Uint8)) -- From array
                 , Ref s (Array 258 (Stored Uint8)) -- To array
                 ]
                :-> Ix 258)
encode = proc "encode" $ \from to -> body $ do
  off <- local (ival (0::Ix 258))
  store (to ! 0) fbo

  o <- deref off
  store off (o+1)

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

  ret =<< deref off

  where
  storeTo to off v = do
    o <- deref off
    store (to ! o) v
    store off (o+1)


