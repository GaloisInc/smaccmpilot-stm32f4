{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.GCSStreamTiming where

import Ivory.Language

gcsStreamTimingTypeModule :: Module
gcsStreamTimingTypeModule = package "gcsstream_timing" $ do
  defStruct (Proxy :: Proxy "gcsstream_timing")

[ivory|
struct gcsstream_timing
  { heartbeat           :: Stored Uint32
  ; servo_output_raw    :: Stored Uint32
  ; attitude            :: Stored Uint32
  ; gps_raw_int         :: Stored Uint32
  ; vfr_hud             :: Stored Uint32
  ; global_position_int :: Stored Uint32
  ; params              :: Stored Uint32
  ; radio               :: Stored Uint32
  }
|]
