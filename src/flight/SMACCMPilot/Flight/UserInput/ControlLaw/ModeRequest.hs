{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.UserInput.ControlLaw.ModeRequest
  ( ModeRequestMachine(..)
  , taskModeRequestMachine
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.ControlLawRequest as R
import qualified SMACCMPilot.Flight.Types.ControlLaw        as L
import qualified SMACCMPilot.Flight.Types.ControlSource     as S
import qualified SMACCMPilot.Flight.Types.ThrottleMode      as T
import qualified SMACCMPilot.Flight.Types.YawMode           as Y

data ModeRequestMachine
  = ModeRequestMachine
    { mrm_init :: forall eff s . Ref s (Struct "control_law") -> Ivory eff ()
    , mrm_ppm  :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    , mrm_mav  :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    , mrm_auto :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    }

taskModeRequestMachine :: Task p ModeRequestMachine
taskModeRequestMachine = do
  ppm_req  <- taskLocal "ppm_req"
  mav_req  <- taskLocal "mav_req"
  auto_req <- taskLocal "auto_req"

  f <- fresh
  let named n = "mode_request_machine_" ++ n ++ "_" ++ (show f)

      init_proc :: Def('[Ref s (Struct "control_law")]:->())
      init_proc = proc (named "init") $ \law -> body $ do
        store (law ~> L.stab_ctl)    S.ppm
        store (law ~> L.thr_mode)    T.direct
        store (law ~> L.autothr_ctl) S.ppm
        -- Force all laws to PPM until we hear otherwise
        store (ppm_req ~> R.set_stab_ppm)    true
        store (ppm_req ~> R.set_thr_direct)  true
        store (ppm_req ~> R.set_autothr_ppm) true

      ppm_proc :: Def('[ Ref      s1 (Struct "control_law")
                       , ConstRef s2 (Struct "control_law_request")
                       ] :-> ())
      ppm_proc = proc (named "ppm") $ \law req -> body $ do
        refCopy ppm_req req
        call_ update law

      mav_proc :: Def('[ Ref      s1 (Struct "control_law")
                       , ConstRef s2 (Struct "control_law_request")
                       ] :-> ())
      mav_proc = proc (named "mav") $ \law req -> body $ do
        refCopy mav_req req
        call_ update law

      auto_proc :: Def('[ Ref      s1 (Struct "control_law")
                        , ConstRef s2 (Struct "control_law_request")
                        ] :-> ())
      auto_proc = proc (named "auto") $ \law req -> body $ do
        refCopy auto_req req
        call_ update law

      update ::  Def('[ Ref s (Struct "control_law") ] :->())
      update = proc (named "update") $ \law -> body $ do
        decide_stab_ctl    >>= store (law ~> L.stab_ctl)
        decide_yaw_mode    >>= store (law ~> L.yaw_mode)
        decide_thr_mode    >>= store (law ~> L.thr_mode)
        decide_autothr_ctl >>= store (law ~> L.autothr_ctl)

      decide_stab_ctl :: Ivory (ProcEffects s ()) S.ControlSource
      decide_stab_ctl = do
        stab_mav_ppm   <- deref (ppm_req ~> R.set_stab_mavlink)
        stab_mav_mav   <- deref (mav_req ~> R.set_stab_mavlink)
        stab_auto_ppm  <- deref (ppm_req ~> R.set_stab_auto)
        stab_auto_auto <- deref (auto_req ~> R.set_stab_auto)
        cond
          [ stab_mav_ppm  .&& stab_mav_mav   ==> return S.mavlink
          , stab_auto_ppm .&& stab_auto_auto ==> return S.auto
          , true ==> return S.ppm -- Always default to PPM
          ]

      decide_yaw_mode :: Ivory (ProcEffects s ()) Y.YawMode
      decide_yaw_mode = do
        stab_src     <- decide_stab_ctl
        yaw_head_ppm  <- deref (ppm_req  ~> R.set_yaw_heading)
        yaw_head_mav  <- deref (mav_req  ~> R.set_yaw_heading)
        yaw_head_auto <- deref (auto_req ~> R.set_yaw_heading)
        cond
          [ stab_src ==? S.ppm     .&& yaw_head_ppm  ==> return Y.heading
          , stab_src ==? S.mavlink .&& yaw_head_mav  ==> return Y.heading
          , stab_src ==? S.auto    .&& yaw_head_auto ==> return Y.heading
          , true ==> return Y.rate
          ]

      decide_thr_mode :: Ivory (ProcEffects s ()) T.ThrottleMode
      decide_thr_mode = do
        thr_direct_ppm <- deref (ppm_req ~> R.set_thr_direct)
        thr_auto_ppm   <- deref (ppm_req ~> R.set_thr_auto)
        thr_auto_mav   <- deref (mav_req ~> R.set_thr_auto)
        thr_auto_auto  <- deref (auto_req ~> R.set_thr_auto)
        cond
          [ thr_direct_ppm ==> return T.direct
          , thr_auto_ppm .&&
              (thr_auto_mav .|| thr_auto_auto) ==> return T.autothrottle
          , true ==> return T.direct
          ]

      -- could improve this code, based directly on stab_ctl with substitution
      decide_autothr_ctl :: Ivory (ProcEffects s ()) S.ControlSource
      decide_autothr_ctl = do
        autothr_mav_ppm   <- deref (ppm_req ~> R.set_autothr_mavlink)
        autothr_mav_mav   <- deref (mav_req ~> R.set_autothr_mavlink)
        autothr_auto_ppm  <- deref (ppm_req ~> R.set_autothr_auto)
        autothr_auto_auto <- deref (auto_req ~> R.set_autothr_auto)
        cond
          [ autothr_mav_ppm  .&& autothr_mav_mav   ==> return S.mavlink
          , autothr_auto_ppm .&& autothr_auto_auto ==> return S.auto
          , true ==> return S.ppm -- always default to ppm
          ]

  taskModuleDef $ do
    incl init_proc
    incl ppm_proc
    incl mav_proc
    incl auto_proc
    incl update
  return ModeRequestMachine
    { mrm_init = call_ init_proc
    , mrm_ppm  = call_ ppm_proc
    , mrm_mav  = call_ mav_proc
    , mrm_auto = call_ auto_proc
    }
