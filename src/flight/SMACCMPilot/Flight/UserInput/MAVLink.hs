{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.MAVLink
  ( mavlinkInputTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Mavlink.Messages.RcChannelsOverride as O

import qualified SMACCMPilot.Flight.Types.UserInput         as UI
import qualified SMACCMPilot.Flight.Types.UISource          as S
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CR

mavlinkInputTower :: ChannelSink (Struct "rc_channels_override_msg")
                  -> ChannelSink (Struct "control_law_request")
                  -> Tower p ( ChannelSink (Struct "userinput_result")
                             , ChannelSink (Struct "control_law_request"))
mavlinkInputTower rcovr_sink mav_req_snk = do
  towerDepends O.rcChannelsOverrideModule
  ui_chan <- channel
  cr_chan <- channel
  task "mavlinkInputTask" $ do
    ui_emitter <- withChannelEmitter (src ui_chan) "ui_emitter"
    cr_emitter <- withChannelEmitter (src cr_chan) "cr_emitter"

    active           <- taskLocal "active"
    last_update_time <- taskLocal "last_update_time"
    taskInit $ do
      store active false

    rcovr_event <- withChannelEvent rcovr_sink "rcoverride"
    handle rcovr_event "rcoverride" $ \ovr_msg -> do
      now <- getTime
      (ui, ui_valid) <- decodeRCOverride ovr_msg now

      lawRequest ui_valid now >>= emit_ cr_emitter
      when ui_valid $ do
        emit_ ui_emitter ui
        store active true
        store last_update_time now

    -- Proxy MAVLink based control law requests to make sure request is
    -- consistent with userinput active state
    mavreq_event <- withChannelEvent mav_req_snk "mav_controllaw_req"
    handle mavreq_event "mav_controllaw_req" $ \req_in -> do
      act <- deref active
      req <- local (istruct [])
      refCopy req req_in
      store (req ~> CR.set_ui_mavlink) act
      emit_ cr_emitter (constRef req)

    onPeriod (Milliseconds 5) $ \now -> do
      act <- deref active
      lst <- deref last_update_time
      when (act .&& ((now - lst) >? timeout)) $ do
        lawRequest false now >>= emit_ cr_emitter
        store active false

  return (snk ui_chan, snk cr_chan)
  where
  timeout = 250

lawRequest :: (GetAlloc eff ~ Scope cs)
           => IBool
           -> ITime
           -> Ivory eff (ConstRef (Stack cs) (Struct "control_law_request"))
lawRequest ui_valid time = do
  cr <- local $ CR.initControlLawRequest
    [ CR.set_ui_mavlink      .= ival ui_valid
    , CR.set_thr_auto        .= ival ui_valid
    , CR.time                .= ival time
    ]
  return (constRef cr)

decodeRCOverride :: (GetAlloc eff ~ Scope cs)
          => ConstRef s (Struct "rc_channels_override_msg")
          -> ITime
          -> Ivory eff (ConstRef (Stack cs) (Struct "userinput_result"), IBool)
decodeRCOverride msg now = do
  (scaled_roll,  valid_roll)  <- scale =<< deref (msg ~> O.chan1_raw)
  (scaled_pitch, valid_pitch) <- scale =<< deref (msg ~> O.chan2_raw)
  (scaled_thr,   valid_thr)   <- scale =<< deref (msg ~> O.chan3_raw)
  (scaled_yaw,   valid_yaw)   <- scale =<< deref (msg ~> O.chan4_raw)
  deadman                     <- deref (msg ~> O.chan5_raw)
  ui <- local $ istruct
    [ UI.throttle .= ival scaled_thr
    , UI.roll     .= ival scaled_roll
    , UI.pitch    .= ival scaled_pitch
    , UI.yaw      .= ival scaled_yaw
    , UI.time     .= ival now
    , UI.source   .= ival S.mavlink
    ]
  ui_valid <- assign $ (deadman ==? 2000)
                   .&& valid_thr
                   .&& valid_roll
                   .&& valid_pitch
                   .&& valid_yaw
  return (constRef ui, ui_valid)
  where
  scale :: Uint16 -> Ivory eff (IFloat, IBool)
  scale raw = do
    a <- assign $ ((safeCast raw) - 1500) / 500
    b <- assign $ raw >=? 1000 .&& raw <=? 2000
    return (a,b)

