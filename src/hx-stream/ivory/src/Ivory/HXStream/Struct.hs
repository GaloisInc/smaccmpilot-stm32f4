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

hxstream_types :: Module
hxstream_types = package "hxstream_types" $ do
  defStruct (Proxy :: Proxy "hxstream_state")
