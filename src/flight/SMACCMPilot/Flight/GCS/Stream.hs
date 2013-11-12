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

-- hide the scope from the typechecker until application. once we have an
-- ivory monad without a codescope parameter, we wont need this or the ugly
-- recursion below
-- newtype SelectorAction =
--   SelectorAction
--     { unwrapSelectorAction :: forall eff
--     . Label "gcsstream_timing" (Struct "gcsstream_data") -> Ivory eff ()
--     }

updateGCSStreamPeriods :: Ref s (Struct "gcsstream_timing")
                       -> Uint8  -- request stream id (mavlink enum typed)
                       -> IBool  -- enabled
                       -> Uint16 -- stream rate in hertz
                       -> Ivory eff ()
updateGCSStreamPeriods periods streamid enabled rate = do
  ifte_ (streamid ==? fromIntegral MavDS.id_ALL)
        (mapM_ setrate allstreams)
        (withSelectorFromId streamid)
  where
  allstreams = [ servo_output_raw
               , attitude
               , gps_raw_int
               , vfr_hud
               , global_position_int
               ]
  setrate :: GcsTimingLabel -> Ivory eff ()
  setrate selector = when enabled (setPeriod selector periods newperiod)
    where
    newperiod = 1000 `iDiv` (safeCast rate)

  withSelectorFromId :: Uint8
--                     -> SelectorAction
                     -> Ivory eff ()
  withSelectorFromId tofind = aux tbl
    where -- explicit recursion and existential quantification is a little weird
          -- not foldr, because of nested block typing which is going away soon
          -- anyway
    aux ::  [(Integer, GcsTimingLabel)]
        -> Ivory eff ()
    aux ((sid, sel):ts) = ifte_ (fromIntegral sid ==? tofind)
                                (setrate sel)
                                (aux ts)
    aux [] = return ()

  tbl ::[(Integer, GcsTimingLabel)]
  tbl = [ (MavDS.id_RAW_CONTROLLER,  servo_output_raw)
        , (MavDS.id_EXTRA1,          attitude)
        , (MavDS.id_POSITION,        global_position_int)
        , (MavDS.id_EXTENDED_STATUS, gps_raw_int)
        , (MavDS.id_EXTRA2,          vfr_hud)
        ]

setNewPeriods :: ConstRef s0 (Struct "gcsstream_timing") -- NEW periods
            -> Ref s1        (Struct "gcsstream_timing") -- STATE periods
            -> Ref s2        (Struct "gcsstream_timing") -- schedule
            -> Uint32                                    -- Now
            -> Ivory eff ()                              -- update schedule
setNewPeriods new state schedule now = do
  mapM_ update selectors
  where
  update :: GcsTimingLabel -> Ivory eff ()
  update selector = do
    n <- getPeriod selector new
    s <- getPeriod selector state
    unless (n ==? s) $ do
      setPeriod selector state n
      setNextTime (constRef state) schedule selector now
  selectors = [ heartbeat, servo_output_raw, attitude, gps_raw_int
              , vfr_hud, global_position_int, params, radio ]

setNextTime :: ConstRef s (Struct "gcsstream_timing") -- periods
            -> Ref s1     (Struct "gcsstream_timing") -- schedule
            -> GcsTimingLabel                         -- selector for packet sent
            -> Uint32                                 -- Now
            -> Ivory eff ()                           -- update schedule
setNextTime periods schedule selector now = do
  per  <- getPeriod selector periods
  prev <- getPeriod selector schedule
  -- Schedule always for a time ahead of now, but keep consistent
  -- period by adding to prev, if possible.
  -- XXX rollover creates a problem here but that should take 49 days
  next <- assign $ (prev + per <? now) ? (now + per, prev + per)
  setPeriod selector schedule next

streamDue :: ConstRef s1 (Struct "gcsstream_timing") -- periods
          -> ConstRef s2 (Struct "gcsstream_timing") -- schedule
          -> GcsTimingLabel                          -- stream selector
          -> Uint32                                  -- last update
          -> Uint32                                  -- now
          -> Ivory eff IBool
streamDue periods schedule selector last now = do
  p <- getPeriod selector periods
  active <- assign (p >? 0)
  duetime <- getPeriod selector schedule
  return (active .&& (duetime >? last).&& (duetime <=? now))

