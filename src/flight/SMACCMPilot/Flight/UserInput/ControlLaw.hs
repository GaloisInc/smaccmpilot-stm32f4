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

controlLawTower :: ChannelSink 16 (Struct "control_law_request")
                -> ChannelSink 16 (Struct "control_law_request")
                -> ChannelSink 16 (Struct "control_law_request")
                -> Tower p (ChannelSink 16 (Struct "control_law"))
controlLawTower ppm_req_snk mavlink_req_snk nav_req_snk  = do
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
      publish law_state

    onChannel ppm_req_snk "ppm_req_snk" $ \ppm_req -> do
      armingPrimaryRequest law_state ppm_req
      mrm_ppm mrm law_state ppm_req
      publish law_state

    onChannel mavlink_req_snk "mavlink_req_snk" $ \mavlink_req -> do
      armingSecondaryRequest law_state mavlink_req
      mrm_mavlink mrm law_state mavlink_req

    onChannel nav_req_snk "nav_req_snk" $ \nav_req -> do
      -- Navigation controller cannot effect arming.
      mrm_nav mrm law_state nav_req
      publish law_state

  return (snk law_chan)
