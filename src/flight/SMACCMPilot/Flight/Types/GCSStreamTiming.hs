{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.GCSStreamTiming where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Flight.Types.GCSStreamStruct

gcsStreamTimingTypeModule :: Module
gcsStreamTimingTypeModule = package "gcsstream_timing" $ do
  defStruct (Proxy :: Proxy "gcsstream_timing")
  defStruct (Proxy :: Proxy "gcsstream_data")

[ivory|

struct gcsstream_timing
  { heartbeat           :: Struct "gcsstream_data"
  ; servo_output_raw    :: Struct "gcsstream_data"
  ; attitude            :: Struct "gcsstream_data"
  ; gps_raw_int         :: Struct "gcsstream_data"
  ; vfr_hud             :: Struct "gcsstream_data"
  ; global_position_int :: Struct "gcsstream_data"
  ; params              :: Struct "gcsstream_data"
  ; radio               :: Struct "gcsstream_data"
  }

|]

--------------------------------------------------------------------------------
-- XXX might be worth being a newtype, but it's a bit overkill here.

isHardRealTime :: IBool -> IBool
isHardRealTime = id

hardRealTime :: IBool
hardRealTime = true

softRealTime :: IBool
softRealTime = false

--------------------------------------------------------------------------------

mkTimingData :: Uint32 -> IBool -> Init (Struct "gcsstream_data")
mkTimingData per hardrt = istruct [ period .= ival per
                                  , hard_deadline .= ival rt
                                  ]
  where rt = isHardRealTime hardrt

type GcsTimingLabel = Label "gcsstream_timing" (Struct "gcsstream_data")

-- | Message period.
getPeriod :: ( IvoryRef ref
             , IvoryExpr (ref s (Struct "gcsstream_timing"))
             , IvoryExpr (ref s (Struct "gcsstream_data"))
             , IvoryExpr (ref s (Stored Uint32))
             )
          => GcsTimingLabel
          -> ref s (Struct "gcsstream_timing")
          -> Ivory eff Uint32
getPeriod l ref = do
  let d = ref ~> l
  return =<< (d ~>* period)

-- | Update message period.
setPeriod :: GcsTimingLabel
          -> Ref s (Struct "gcsstream_timing")
          -> Uint32
          -> Ivory eff ()
setPeriod l ref per = store (ref ~> l ~> period) per

-- | Hard real-time or soft real-time?
getDeadline :: ( IvoryRef ref
             , IvoryExpr (ref s (Struct "gcsstream_timing"))
             , IvoryExpr (ref s (Struct "gcsstream_data"))
             , IvoryExpr (ref s (Stored IBool))
             )
          => GcsTimingLabel
          -> ref s (Struct "gcsstream_timing")
          -> Ivory eff IBool
getDeadline l ref = do
  let d = ref ~> l
  return =<< (d ~>* hard_deadline)

--------------------------------------------------------------------------------
