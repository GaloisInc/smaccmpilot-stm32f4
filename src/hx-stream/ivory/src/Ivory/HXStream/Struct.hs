{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.HXStream.Struct where

import Ivory.Language
import Ivory.HXStream.Types

type Hx  = Struct "hxstream_state"

[ivory|
struct hxstream_state
  { state   :: Stored HXState
  ; offset  :: Stored Sint32
  -- Frame tag
  ; ftag    :: Stored Uint8
  }
|]

