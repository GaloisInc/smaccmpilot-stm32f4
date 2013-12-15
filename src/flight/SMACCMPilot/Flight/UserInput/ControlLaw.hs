{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.ControlLaw
  ( controlLawTower
  ) where

import Ivory.Language
import Ivory.Tower

import qualified SMACCMPilot.Flight.Types.ControlLawRequest as R
import qualified SMACCMPilot.Flight.Types.ControlLaw        as L

import           SMACCMPilot.Flight.UserInput.ControlLaw.ArmingRequest
import           SMACCMPilot.Flight.UserInput.ControlLaw.ModeRequest

controlLawTower :: ChannelSink 16 (Struct "control_law_request")
                -> ChannelSink 16 (Struct "control_law_request")
                -> Tower p (ChannelSink 16 (Struct "control_law"))
controlLawTower ppm_req_snk mav_req_snk = do
  law_chan <- channel
  task "controlLawTask" $ do
    law_emitter <- withChannelEmitter (src law_chan) "law_emitter"
    law_state   <- taskLocal "law_state"
    mrm <- taskModeRequestMachine
    m <- withGetTimeMillis

    let publish :: (GetAlloc eff ~ Scope cs)
                => Ref s (Struct "control_law") -> Ivory eff ()
        publish law = do
          t <- getTimeMillis m
          store (law ~> L.time) t
          emit_ law_emitter (constRef law)

    taskInit $ do
      armingInit law_state
      mrm_init mrm law_state
      auto_law <- local ival_auto_law
      mrm_auto mrm law_state (constRef auto_law)
      publish law_state

    onChannel ppm_req_snk "ppm_req_snk" $ \ppm_req -> do
      armingPrimaryRequest law_state ppm_req
      mrm_ppm mrm law_state ppm_req
      publish law_state

    onChannel mav_req_snk "mav_req_snk" $ \mav_req -> do
      armingSecondaryRequest law_state mav_req
      mrm_mav mrm law_state mav_req
      publish law_state

  return (snk law_chan)
  where
  -- Autonomous mode law permits using autothr with ppm or mavlink input.
  -- In the future, there will be a variety of autonomous modes to allow an
  -- internal flight plan to control the stabilizer and autothrottle, but for
  -- now, this law is effectively a constant.
  ival_auto_law = istruct
    [ R.set_safe             .= ival false
    , R.set_disarmed         .= ival false
    , R.set_armed            .= ival false
    , R.set_stab_ppm         .= ival false
    , R.set_stab_mavlink     .= ival false
    , R.set_stab_auto        .= ival false
    , R.set_thr_direct       .= ival false
    , R.set_thr_auto         .= ival true
    , R.set_autothr_ppm      .= ival true
    , R.set_autothr_mavlink  .= ival true
    , R.set_autothr_auto     .= ival false
    , R.time                 .= ival 0
    ]
