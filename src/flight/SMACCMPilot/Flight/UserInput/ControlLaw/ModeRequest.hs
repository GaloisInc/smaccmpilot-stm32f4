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
import qualified SMACCMPilot.Flight.Types.UISource          as U
import qualified SMACCMPilot.Flight.Types.ThrottleMode      as T
import qualified SMACCMPilot.Flight.Types.YawMode           as Y

data ModeRequestMachine
  = ModeRequestMachine
    { mrm_init :: forall eff s . Ref s (Struct "control_law") -> Ivory eff ()
    , mrm_ppm  :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    , mrm_mavlink :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    , mrm_nav :: forall eff s1 s2 . Ref      s1 (Struct "control_law")
                                  -> ConstRef s2 (Struct "control_law_request")
                                  -> Ivory eff ()
    }

taskModeRequestMachine :: Task p ModeRequestMachine
taskModeRequestMachine = do
  ppm_req  <- taskLocal "ppm_req"
  mavlink_req  <- taskLocal "mavlink_req"
  nav_req <- taskLocal "nav_req"

  f <- fresh
  let named n = "mode_request_machine_" ++ n ++ "_" ++ (show f)

      init_proc :: Def('[Ref s (Struct "control_law")]:->())
      init_proc = proc (named "init") $ \law -> body $ do
        store (law ~> L.ui_source)      U.ppm
        store (law ~> L.yaw_mode)       Y.rate
        store (law ~> L.thr_mode)       T.direct
        store (law ~> L.autothr_source) S.ui
        store (law ~> L.stab_source)    S.ui
        store (law ~> L.head_source)    S.ui
        -- Force all laws to PPM UI until we hear otherwise
        store (ppm_req ~> R.set_ui_ppm)         true
        store (ppm_req ~> R.set_thr_direct)     true
        store (ppm_req ~> R.set_autothr_src_ui) true
        store (ppm_req ~> R.set_stab_src_ui)    true
        store (ppm_req ~> R.set_head_src_ui)    true

      ppm_proc :: Def('[ Ref      s1 (Struct "control_law")
                       , ConstRef s2 (Struct "control_law_request")
                       ] :-> ())
      ppm_proc = proc (named "ppm") $ \law req -> body $ do
        refCopy ppm_req req
        call_ update law

      mavlink_proc :: Def('[ Ref      s1 (Struct "control_law")
                           , ConstRef s2 (Struct "control_law_request")
                           ] :-> ())
      mavlink_proc = proc (named "mavlink") $ \law req -> body $ do
        refCopy mavlink_req req
        call_ update law

      nav_proc :: Def('[ Ref      s1 (Struct "control_law")
                       , ConstRef s2 (Struct "control_law_request")
                       ] :-> ())
      nav_proc = proc (named "nav") $ \law req -> body $ do
        refCopy nav_req req
        call_ update law

      update ::  Def('[ Ref s (Struct "control_law") ] :->())
      update = proc (named "update") $ \law -> body $ do
        ui_src <- decide_ui_source
        store (law ~> L.ui_source) ui_src
        decide_yaw_mode ui_src >>= store (law ~> L.yaw_mode)
        decide_thr_mode ui_src >>= store (law ~> L.thr_mode)
        decide_autothr_source  >>= store (law ~> L.autothr_source)
        decide_stab_source     >>= store (law ~> L.stab_source)
        decide_head_source     >>= store (law ~> L.head_source)

      decide_control_source :: Label "control_law_request" (Stored IBool)
                            -> Label "control_law_request" (Stored IBool)
                            -> Ivory (ProcEffects s ()) S.ControlSource
      decide_control_source ui_lbl nav_lbl = do
        ppm_allows    <- deref (ppm_req ~> nav_lbl)
        nav_allows    <- deref (nav_req ~> nav_lbl)
        nav_prohibits <- deref (nav_req ~> ui_lbl)
        cond
          [ ppm_allows .&& nav_allows .&& iNot nav_prohibits ==> return S.nav
          , true ==> return S.ui
          ]

      decide_autothr_source = decide_control_source
                                R.set_autothr_src_ui
                                R.set_autothr_src_nav
      decide_stab_source = decide_control_source
                                R.set_stab_src_ui
                                R.set_stab_src_nav
      decide_head_source = decide_control_source
                                R.set_head_src_ui
                                R.set_head_src_nav

      decide_ui_source :: Ivory (ProcEffects s ()) U.UISource
      decide_ui_source = do
        -- Default to PPM input unless both PPM and MAVLink assert MAVLink ui
        ui_mav_ppm <- deref (ppm_req ~> R.set_ui_mavlink)
        ui_mav_mav <- deref (mavlink_req ~> R.set_ui_mavlink)
        cond
          [ ui_mav_ppm .&& ui_mav_mav ==> return U.mavlink
          , true ==> return U.ppm
          ]

      decide_yaw_mode :: U.UISource -> Ivory (ProcEffects s ()) Y.YawMode
      decide_yaw_mode ui_src = do
        -- Use the yaw mode requested by the UI Source
        yaw_head_ppm  <- deref (ppm_req ~> R.set_yaw_heading)
        yaw_head_mav  <- deref (mavlink_req ~> R.set_yaw_heading)
        cond
          [ ui_src ==? U.ppm     .&& yaw_head_ppm  ==> return Y.heading
          , ui_src ==? U.mavlink .&& yaw_head_mav  ==> return Y.heading
          , true ==> return Y.rate
          ]

      decide_thr_mode :: U.UISource -> Ivory (ProcEffects s ()) T.ThrottleMode
      decide_thr_mode ui_src = do
        -- Use the throttle mode requested by the UI Source
        thr_direct_ppm <- deref (ppm_req ~> R.set_thr_direct)
        thr_auto_ppm   <- deref (ppm_req ~> R.set_thr_auto)
        thr_auto_mav   <- deref (mavlink_req ~> R.set_thr_auto)
        cond
          [ ui_src ==? U.ppm     .&& thr_auto_ppm ==> return T.autothrottle
          , ui_src ==? U.mavlink .&& thr_auto_mav ==> return T.autothrottle
          , true ==> return T.direct
          ]

  taskModuleDef $ do
    incl init_proc
    incl ppm_proc
    incl mavlink_proc
    incl nav_proc
    incl update
  return ModeRequestMachine
    { mrm_init = call_ init_proc
    , mrm_ppm  = call_ ppm_proc
    , mrm_mavlink  = call_ mavlink_proc
    , mrm_nav = call_ nav_proc
    }
