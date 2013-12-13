{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.ControlLaw
  ( controlLawTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as R
import qualified SMACCMPilot.Flight.Types.ControlLaw        as L
import qualified SMACCMPilot.Flight.Types.ArmedMode         as A
import qualified SMACCMPilot.Flight.Types.ControlSource     as S
import qualified SMACCMPilot.Flight.Types.ThrottleMode      as T

controlLawTower :: ChannelSink 16 (Struct "control_law_request")
                -> ChannelSink 16 (Struct "control_law_request")
                -> Tower p (ChannelSink 16 (Struct "control_law"))
controlLawTower ppm_req_snk mav_req_snk = do
  law <- channel
  task "controlLawTask" $ do
    law_emitter <- withChannelEmitter (src law) "law_emitter"
    law_state   <- taskLocal "law_state"
    taskInit $ do
      store (law_state ~> L.armed_mode)  A.safe
      store (law_state ~> L.stab_ctl)    S.ppm
      store (law_state ~> L.thr_mode)    T.direct
      store (law_state ~> L.autothr_ctl) S.ppm
      store (law_state ~> L.time)        0
      emit_ law_emitter (constRef law_state)

    onChannel ppm_req_snk "ppm_req_snk" $ \ppm_req -> do
      -- ARMING LOGIC
      armed_mode  <- deref (law_state ~> L.armed_mode)
      safe_req    <- deref (ppm_req ~> R.set_safe)
      arm_req     <- deref (ppm_req ~> R.set_armed)
      disarm_req  <- deref (ppm_req ~> R.set_disarmed)
      cond_
        [ -- Safe mode must be asserted at all times, otherwise we default
          -- to disarmed.
          armed_mode ==? A.safe .&& iNot safe_req ==>
            store (law_state ~> L.armed_mode) A.disarmed
          -- Never transition from safe to armed on a single event. PPM machine
          -- should enforce a delay between lifting the deadman switch &
          -- completing an arming sequence, but we'll ensure it again here.
        , true ==> cond_
            -- When armed or disarmed, Safe requests dominate, then disarm
            -- requests, then arm requests.
            [ safe_req   ==> store (law_state ~> L.armed_mode) A.safe
            , disarm_req ==> store (law_state ~> L.armed_mode) A.disarmed
            , arm_req    ==> store (law_state ~> L.armed_mode) A.armed
            ]
        ]
      emit_ law_emitter (constRef law_state)

    onChannel mav_req_snk "mav_req_snk" $ \mav_req -> do
      return ()

  return (snk law)

