{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Stream where

import Ivory.Language

import SMACCMPilot.Flight.Types.GCSStreamTiming

defaultPeriods :: Init (Struct "gcsstream_timing")
defaultPeriods =
  istruct
    [ heartbeat            .= ival 1000
    , servo_output_raw     .= ival 0
    , attitude             .= ival 0
    , gps_raw_int          .= ival 0
    , vfr_hud              .= ival 0
    , global_position_int  .= ival 0
    , params               .= ival 100
    ]


setNextTime :: ConstRef s (Struct "gcsstream_timing") -- periods
            -> Ref s1 (Struct "gcsstream_timing") -- schedule
            -> Label "gcsstream_timing" (Stored Uint32) -- selector for packet sent
            -> Uint32 -- Now
            -> Ivory s2 () () -- update schedule

setNextTime periods schedule selector now = do
  per <- deref (periods ~> selector)
  prev <- deref (schedule ~> selector)
  -- Schedule always for a time ahead of now, but keep consistent
  -- period by adding to prev, if possible.
  -- XXX rollover creates a problem here but that should take 49 days
  next <- assign $ ((prev + per) <? now ) ? (now + per, prev + per)
  store (schedule ~> selector) next

streamDue :: ConstRef s1 (Struct "gcsstream_timing")    -- periods
          -> ConstRef s2 (Struct "gcsstream_timing")    -- schedule
          -> Label "gcsstream_timing" (Stored Uint32) -- stream selector
          -> Uint32  -- last update
          -> Uint32  -- now
          -> Ivory s3 () IBool
streamDue periods schedule selector last now = do
  p <- deref (periods ~> selector)
  active <- assign (p ==? 0)
  duetime <- deref (schedule ~> selector)
  return (active .&& (duetime >? last).&& (duetime <=? now))


nextDueTime :: ConstRef s1 (Struct "gcsstream_timing")    -- periods
            -> ConstRef s2 (Struct "gcsstream_timing")    -- schedule
            -> Uint32  -- now
            -> Ivory s3 () Uint32
nextDueTime _periods _schedule now = do
  -- XXX should implement this later.
  return (now + 100)

