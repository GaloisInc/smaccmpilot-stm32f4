{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.HXStream.Types where

import Ivory.Language

hxstreamTypeModule :: Module
hxstreamTypeModule = package "hxstream_type" $ do
  defStruct (Proxy :: Proxy "hxstream_state")

hxstream_fstate_Begin    :: Uint8
hxstream_fstate_Begin    = 0
hxstream_fstate_Progress :: Uint8
hxstream_fstate_Progress = 1
hxstream_fstate_Complete :: Uint8
hxstream_fstate_Complete = 2

[ivory|
struct hxstream_state
  { buf     :: Array 128 (Stored Uint8)
  ; offset  :: Stored Sint32
  ; fstate  :: Stored Uint8
  ; escaped :: Stored IBool
  }
|]


