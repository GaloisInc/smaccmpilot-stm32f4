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
-- Otherwise, store the bytes.
appendFrame :: Uint8 -> Ref s Hx -> Ivory eff ()
appendFrame b s = do
  ix <- deref (s ~> offset)
  let arr = (s ~> buf)
  ifte_ (ix >=? arrayLen arr)
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
decodeSM :: Def ('[Ref s Hx, Uint8] :-> IBool)
decodeSM = proc "decodeSM" $ \state b -> body $ do
  sta <- deref (state ~> fstate)
  esc <- deref (state ~> escaped)
  cond_
    [   (sta ==? hxstream_fstate_Begin) .&& (b ==? fbo)
    ==> (emptyStreamState state >> setEscaped false state)
    ,   sta ==? hxstream_fstate_Progress
    ==> progress state b esc
    ,   sta ==? hxstream_fstate_Complete
    ==> return ()
    ,   true
    ==> emptyStreamState state
    ]
  s <- deref (state ~> fstate)
  ret (s ==? hxstream_fstate_Complete)
    where
    progress state b esc =
      cond_ [ esc       ==> (   setEscaped false state
                             >> appendFrame (escape b) state)
            , b ==? ceo ==> setEscaped true state
            , b ==? fbo ==> setComplete state
            , true      ==> appendFrame b state
            ]

-- | Decode an hxstreamed array into the raw bytes.  The input array can be
-- arbitrary size.  Decoding stops if (1) the hxstream state we're decoding into
-- is full (2) a stop byte is encountered (the input array may contain hxstream
-- frame boundaries).  An index to the first byte not decoded is returned.  The
-- index may overflow, and point to 0 if the full input array is processed.
decode :: SingI n
       => Def ( '[ Ref s (Array n (Stored Uint8))
                 , Ref s Hx
                 ] :-> Ix n)
decode = proc "decode" $ \from state -> body $ do
  arrayMap $ \ix -> do
    v    <- deref (from ! ix)
    b    <- call decodeSM state v
    over <- deref (state ~> ovf)
    -- Couldn't put the current byte in---we overflowed.
    when over (ret ix)
    -- Won't put the next byte in---we're done.
    when b (ret (ix+1))
  -- Return index 0 if all went well.
  ret 0

hxstreamTypeModule :: Module
hxstreamTypeModule = package "hxstream_type" $
  defStruct (Proxy :: Proxy "hxstream_state")

-- | Encode a 128 byte array into a 258 byte array.  This guarantees we have
-- enough storage to hold the 128 bytes.
encode :: Def ( '[ Ref s (Array 128 (Stored Uint8)) -- From array
                 , Ref s' (Array 258 (Stored Uint8)) -- To array
                 ]
                 :-> ())
encode = proc "encode" $ \from to -> body $ do
  off <- local (ival (0::Ix 258))
  -- start byte
  store (to ! 0) fbo

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


