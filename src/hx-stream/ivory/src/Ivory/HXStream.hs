{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.HXStream where

import Ivory.Language
import Ivory.Stdlib

import Ivory.HXStream.Types

emptyStreamState :: Ref s (Struct "hxstream_state") -> Ivory eff ()
emptyStreamState state = do
  store (state ~> offset)  0
  store (state ~> fstate)  hxstream_fstate_Begin
  store (state ~> escaped) false

appendFrame :: Uint8 -> Ref s (Struct "hxstream_state") -> Ivory eff ()
appendFrame b s = return () -- XXX

setEscaped :: IBool -> Ref s (Struct "hxstream_state") -> Ivory eff ()
setEscaped b s = do
  store (s ~> fstate) hxstream_fstate_Progress
  store (s ~> escaped) b

setComplete :: Ref s (Struct "hxstream_state") -> Ivory eff ()
setComplete s =
  store (s ~> fstate) hxstream_fstate_Complete

complete :: Ref s (Struct "hxstream_state") -> Ivory eff IBool
complete state = do
  sta <- deref (state ~> fstate)
  return (sta ==? hxstream_fstate_Complete)

fbo :: Uint8
fbo  = 0x7e

ceo :: Uint8
ceo  = 0x7c

escape :: Uint8 -> Uint8
escape b = b .^ 0x20 -- XOR with 0x20

decodeSM :: Ref s (Struct "hxstream_state")
         -> Uint8
         -> Ivory eff IBool
decodeSM state b = do
  sta <- deref (state ~> fstate)
  off <- deref (state ~> offset)
  esc <- deref (state ~> escaped)

  (ifte_ (sta ==? hxstream_fstate_Begin)
       (when (b ==? fbo)
        (emptyStreamState state >> setEscaped false state))
   (ifte_ (sta ==? hxstream_fstate_Progress)
       (ifte_ (esc) (setEscaped false state >> appendFrame (escape b) state)
        (ifte_ (b ==? ceo) (setEscaped true state)
         (ifte_ (b ==? fbo) (setComplete state)
          {- else -}       (appendFrame b state))))
     (ifte_ (sta ==? hxstream_fstate_Complete)
        (return ()) -- Do nothing.
       ({- IMPOSSIBLE: restore sanity. -} emptyStreamState state))))

  deref (state ~> fstate) >>= \s -> return (s ==? hxstream_fstate_Complete)

encodeK :: (SingI n, eff `AllocsIn` cs)
        => Ref s (Array n (Stored Uint8))
        -> (Uint8 -> Ivory eff IBool) -- Returns true if successful
        -> Ivory eff IBool -- Returns true if successful
encodeK input k = do
  afail <- local (ival false)
  let send b = do
        success <- k b
        ifte success (return ()) (store afail true)
        return success

  arrayMap $ \i -> do
    v <- deref (input ! i)
    ifte_ ((v ==? fbo) .|| (v ==? ceo))
      (send ceo >>= \s ->
       ifte_ (s)
         (send (escape v) >>= \s' ->
          unless (s') (breakOut))
         (breakOut))
      (send v >>= \s ->
       unless (s) breakOut)

  f <- deref afail
  return (iNot afail)


