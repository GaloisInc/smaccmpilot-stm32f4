{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.Flight.GCS.Stream where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Stdlib

import qualified SMACCMPilot.Mavlink.Enums.MavDataStreams as MavDS

import SMACCMPilot.Flight.Types.GCSStreamTiming

--------------------------------------------------------------------------------

defaultPeriods :: Init (Struct "gcsstream_timing")
defaultPeriods =
  istruct
    [ heartbeat            .= mkTimingData 1000 hardRealTime
    , servo_output_raw     .= mkTimingData 0    softRealTime
    , attitude             .= mkTimingData 0    softRealTime
    , gps_raw_int          .= mkTimingData 0    softRealTime
    , vfr_hud              .= mkTimingData 0    softRealTime
    , global_position_int  .= mkTimingData 0    softRealTime
    , params               .= mkTimingData 100  softRealTime
    , radio                .= mkTimingData 1000 softRealTime
    ]

-- | Update the stream period for one (or more) MAVLink streams.  This is called
-- by a GCS Rx handler.
updateGCSStreamPeriods :: Ref s (Struct "gcsstream_timing")
                       -> Uint8  -- request stream id (mavlink enum typed)
                       -> IBool  -- enabled
                       -> Uint16 -- stream rate in hertz
                       -> Ivory eff ()
updateGCSStreamPeriods periods streamid enabled rate = do
  ifte_ (streamid ==? fromIntegral MavDS.id_ALL)
        (mapM_ setrate allstreams)
        (setRateOne streamid)
  where
  allstreams = [ servo_output_raw
               , attitude
               , gps_raw_int
               , vfr_hud
               , veh_commsec
               , global_position_int
               ]

  setrate :: GcsTimingLabel -> Ivory eff ()
  setrate selector = ifte_ (enabled .&& rate >? 0)
                           (setPeriod selector periods newperiod)
                           (setPeriod selector periods 0)
    where
    newperiod = 1000 `iDiv` (safeCast rate)

  setRateOne :: Uint8 -> Ivory eff ()
  setRateOne toFind = mapM_ aux tbl
    where
    aux :: (Integer, GcsTimingLabel) -> Ivory eff ()
    aux (i,l) = when (fromIntegral i ==? toFind) (setrate l)

  tbl ::[(Integer, GcsTimingLabel)]
  tbl = [ (MavDS.id_RAW_CONTROLLER,  servo_output_raw)
        , (MavDS.id_EXTRA1,          attitude)
        , (MavDS.id_POSITION,        global_position_int)
        , (MavDS.id_EXTENDED_STATUS, gps_raw_int)
        , (MavDS.id_EXTRA2,          vfr_hud)
        ]

-- | Take a new set of stream rates and update the current stream rates if they
-- differ.  Also update the schedule based on the new rates.
setNewPeriods :: ConstRef s0 (Struct "gcsstream_timing")   -- NEW periods
              -> Ref s1      (Struct "gcsstream_timing")   -- STATE periods
              -> Ref s2      (Struct "gcsstream_schedule") -- schedule
              -> Uint32                                    -- Now
              -> Ivory eff ()                              -- update schedule
setNewPeriods new state schedule now =
  mapM_ update allTimingLabels
  where
  update :: GcsTimingLabel -> Ivory eff ()
  update selector = do
    n <- getPeriod selector new
    s <- getPeriod selector state
    unless (n ==? s) $ do
      setPeriod selector state n
      setNextTime (constRef state) schedule selector now
    rt <- getDeadline selector new
    setDeadline selector state rt

-- Update the schedule for the stream.  Has the property that the schedule is at
-- least as long as the period set in the gcsstream_timing, but it may exceed
-- it.
setNextTime :: ConstRef s (Struct "gcsstream_timing")   -- periods
            -> Ref s1     (Struct "gcsstream_schedule") -- schedule
            -> GcsTimingLabel                           -- selector for packet sent
            -> Uint32                                   -- Now
            -> Ivory eff ()                             -- update schedule
setNextTime periods schedule selector now = do
  per  <- getPeriod selector periods
  let schedSelector = toSchedLabel selector
  prev <- schedule ~>* schedSelector
  -- Schedule always for a time ahead of now, but keep consistent
  -- period by adding to prev, if possible.
  -- Rollover creates a problem here but that should take 49 days
  next <- assign $ (prev + per <? now) ? (now + per, prev + per)
  -- the stream is inactive or it's scheduled for the future.
  --
  -- XXX the user
  -- might do something bad like enter a 0 schedule on purpose, so I don't know
  -- if we want to die based on user input.
  -- p <- getPeriod selector periods
  -- assert ((p ==? 0) .|| (now <? next))
  store (schedule ~> schedSelector) next

-- Stream is due if the stream is active and the current time exceeds its due
-- time.
streamDue :: ConstRef s1 (Struct "gcsstream_timing")   -- periods
          -> ConstRef s2 (Struct "gcsstream_schedule") -- schedule
          -> GcsTimingLabel                            -- stream selector
          -> Uint32                                    -- now
          -> Ivory eff IBool
streamDue periods schedule selector now = do
  p       <- getPeriod selector periods
  active  <- assign (p >? 0)
  duetime <- schedule ~>* toSchedLabel selector
  return (active .&& (duetime <=? now))

