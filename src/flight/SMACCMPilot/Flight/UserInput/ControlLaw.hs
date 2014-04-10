{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.ControlLaw
  ( controlLawTower
  ) where

import Ivory.Language
import Ivory.Tower

-- import qualified SMACCMPilot.Flight.Types.ControlLawRequest as R
import qualified SMACCMPilot.Flight.Types.ControlLaw        as L

import           SMACCMPilot.Flight.UserInput.ControlLaw.ArmingRequest
import           SMACCMPilot.Flight.UserInput.ControlLaw.ModeRequest

controlLawTower :: ChannelSink (Struct "control_law_request")
                -> ChannelSink (Struct "control_law_request")
                -> ChannelSink (Struct "control_law_request")
                -> Tower p (ChannelSink (Struct "control_law"))
controlLawTower ppm_req_snk mavlink_req_snk nav_req_snk  = do
  law_chan <- channel
  task "controlLawTask" $ do
    law_emitter <- withChannelEmitter (src law_chan) "law_emitter"
    law_state   <- taskLocal "law_state"
    mrm <- taskModeRequestMachine

    let publish :: (GetAlloc eff ~ Scope cs)
                => Ref s (Struct "control_law") -> Ivory eff ()
        publish law = do
          t <- getTime
          store (law ~> L.time) t
          emit_ law_emitter (constRef law)

    taskInit $ do
      armingInit law_state
      mrm_init mrm law_state
      publish law_state

    ppm_req_evt <- withChannelEvent ppm_req_snk "ppm_req_snk"
    handle ppm_req_evt "ppm_req" $ \ppm_req -> do
      armingPrimaryRequest law_state ppm_req
      mrm_ppm mrm law_state ppm_req
      publish law_state

    mav_req_evt <- withChannelEvent mavlink_req_snk "mavlink_req_snk"
    handle mav_req_evt "mavlink_req" $ \mavlink_req -> do
      armingSecondaryRequest law_state mavlink_req
      mrm_mavlink mrm law_state mavlink_req

    nav_req_evt <- withChannelEvent nav_req_snk "nav_req_snk"
    handle nav_req_evt "nav_req" $ \nav_req -> do
      -- Navigation controller cannot effect arming.
      mrm_nav mrm law_state nav_req
      publish law_state

  return (snk law_chan)
