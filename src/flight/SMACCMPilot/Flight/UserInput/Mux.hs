{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.Mux
  ( uiMuxTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput     as UI
import qualified SMACCMPilot.Flight.Types.UISource      as S
import qualified SMACCMPilot.Flight.Types.ControlLaw    as L


uiMuxTower :: ChannelSink 16 (Struct "control_law")
           -> ChannelSink 16 (Struct "userinput_result")
           -> ChannelSink 16 (Struct "userinput_result")
           -> Tower p (ChannelSink 16 (Struct "userinput_result"))
uiMuxTower law_snk ppm_ui_snk mavlink_ui_snk = do
  ui_chan <- channel
  task "uiMuxTask" $ do
    f <- fresh
    got_law      <- taskLocal "got_law"
    last_law     <- taskLocal "last_law"
    got_ppm      <- taskLocal "got_ppm"
    last_ppm     <- taskLocal "last_ppm"
    got_mavlink  <- taskLocal "got_mavlink"
    last_mavlink <- taskLocal "last_mavlink"
    ui_emitter   <- withChannelEmitter (src ui_chan) "ui_emitter"
    taskInit $ do
      store got_law     false
      store got_ppm     false
      store got_mavlink false

    let update = call_ update_proc
        update_proc :: Def('[]:->())
        update_proc = proc ("userinput_mux_update_" ++ show f) $ body $ do
          v_law <- deref got_law
          v_ppm <- deref got_ppm
          v_mav <- deref got_mavlink
          when (v_law .&& v_ppm) $ do

            mav_throttle <- deref (last_mavlink ~> UI.throttle)
            mav_roll     <- deref (last_mavlink ~> UI.roll)
            mav_pitch    <- deref (last_mavlink ~> UI.pitch)
            mav_yaw      <- deref (last_mavlink ~> UI.yaw)
            ppm_throttle <- deref (last_ppm     ~> UI.throttle)
            ppm_roll     <- deref (last_ppm     ~> UI.roll)
            ppm_pitch    <- deref (last_ppm     ~> UI.pitch)
            ppm_yaw      <- deref (last_ppm     ~> UI.yaw)
            ppm_time     <- deref (last_ppm     ~> UI.time)

            ui_source   <- deref (last_law ~> L.ui_source)
            ui_from_mav <- assign $ v_mav .&& ui_source ==? S.mavlink

            ui_out <- local $ istruct
              [ UI.throttle .= ival (ui_from_mav ? (mav_throttle, ppm_throttle))
              , UI.roll     .= ival (ui_from_mav ? (mav_roll,     ppm_roll))
              , UI.pitch    .= ival (ui_from_mav ? (mav_pitch,    ppm_pitch))
              , UI.yaw      .= ival (ui_from_mav ? (mav_yaw,      ppm_yaw))
              , UI.source   .= ival (ui_from_mav ? (S.mavlink, S.ppm))
              , UI.time     .= ival ppm_time
              ]

            emit_ ui_emitter (constRef ui_out)

    taskModuleDef $ incl update_proc

    onChannel law_snk "law" $ \law -> do
      store got_law true
      refCopy last_law law
      update

    onChannel ppm_ui_snk "ppm_ui" $ \ppm_ui -> do
      store got_ppm true
      refCopy last_ppm ppm_ui
      update

    onChannel mavlink_ui_snk "mavlink_ui" $ \mavlink_ui -> do
      store got_mavlink true
      refCopy last_mavlink mavlink_ui
      update

  return (snk ui_chan)
