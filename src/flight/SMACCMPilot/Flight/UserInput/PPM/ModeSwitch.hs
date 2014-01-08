{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.UserInput.PPM.ModeSwitch
  ( ModeSwitch(..)
  , taskModeSwitch
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Flight.Types.UserInput         as I
import qualified SMACCMPilot.Flight.Types.ControlLawRequest as CL

data ModeSwitch =
  ModeSwitch
    { ms_init       :: forall eff   . Ivory eff ()
    , ms_new_sample :: forall eff s . Ref s I.PPMs -> Uint32 -> Ivory eff ()
    , ms_no_sample  :: forall eff   . Ivory eff ()
    , ms_get_cl_req :: forall eff s . Ref s (Struct "control_law_request")
                                   -> Ivory eff ()
    }

newtype ThreePositionSwitch = ThreePositionSwitch Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit)

posUp :: ThreePositionSwitch
posUp = ThreePositionSwitch 0
posCenter :: ThreePositionSwitch
posCenter = ThreePositionSwitch 1
posDown :: ThreePositionSwitch
posDown = ThreePositionSwitch 2

taskModeSwitch :: Task p ModeSwitch
taskModeSwitch = do
  fr <- fresh
  md_last_position      <- taskLocal "md_last_position"
  md_last_position_time <- taskLocal "md_last_position_time"
  let named n = "ppmdecoder_modeswitch_" ++ n ++ "_" ++ show fr

      init_proc :: Def('[]:->())
      init_proc = proc (named "init") $ body $ do
        store md_last_position posDown
        store md_last_position_time 0

      new_sample_proc :: Def('[Ref s I.PPMs, Uint32]:->())
      new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
        switch <- deref (ppms ! (4 :: Ix 8))
        position <- assign $ modeswitchpos_from_ppm switch
        -- XXX debouncing
        store md_last_position position
        store md_last_position_time time

      -- No failure logic--handled in arming state machine.
      no_sample_proc :: Def('[] :-> ())
      no_sample_proc = proc (named "no_sample") $ body $ return ()

      get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
      get_cl_req_proc = proc (named "cl_req_proc") $ \cl_req -> body $ do
        deref md_last_position_time >>= store (cl_req ~> CL.time)
        p <- deref md_last_position
        cond_
          [ p ==? posUp ==> do
              -- UI source may be mavlink
              store (cl_req ~> CL.set_ui_ppm)       true
              store (cl_req ~> CL.set_ui_mavlink)   true
              -- Yaw law must be heading
              store (cl_req ~> CL.set_yaw_rate)     false
              store (cl_req ~> CL.set_yaw_heading)  true
              -- Throttle law must be autothrottle:
              store (cl_req ~> CL.set_thr_direct)   false
              store (cl_req ~> CL.set_thr_auto)     true
              -- Autothrottle input may be ui or nav:
              store (cl_req ~> CL.set_autothr_src_ui)  true
              store (cl_req ~> CL.set_autothr_src_nav) true
              -- Stab input may be ui or nav:
              store (cl_req ~> CL.set_stab_src_ui)  true
              store (cl_req ~> CL.set_stab_src_nav) true
              -- Heading input may be ui or nav:
              store (cl_req ~> CL.set_head_src_ui)  true
              store (cl_req ~> CL.set_head_src_nav) true
          , p ==? posCenter ==> do
              -- UI source must be ppm
              store (cl_req ~> CL.set_ui_ppm)       true
              store (cl_req ~> CL.set_ui_mavlink)   false
              -- Yaw law must be rate
              store (cl_req ~> CL.set_yaw_rate)     true
              store (cl_req ~> CL.set_yaw_heading)  false
              -- Throttle law must be autothrottle:
              store (cl_req ~> CL.set_thr_direct)   false
              store (cl_req ~> CL.set_thr_auto)     true
              -- Autothrottle input must be ui
              store (cl_req ~> CL.set_autothr_src_ui)  true
              store (cl_req ~> CL.set_autothr_src_nav) false
              -- Stab input must be ui:
              store (cl_req ~> CL.set_stab_src_ui)  true
              store (cl_req ~> CL.set_stab_src_nav) false
              -- Heading input irrevelent
              store (cl_req ~> CL.set_head_src_ui)  false
              store (cl_req ~> CL.set_head_src_nav) false
          , p ==? posDown ==> do
              -- UI source must be ppm
              store (cl_req ~> CL.set_ui_ppm)       true
              store (cl_req ~> CL.set_ui_mavlink)   false
              -- Yaw law must be rate
              store (cl_req ~> CL.set_yaw_rate)     true
              store (cl_req ~> CL.set_yaw_heading)  false
              -- Throttle law must be direct:
              store (cl_req ~> CL.set_thr_direct)   true
              store (cl_req ~> CL.set_thr_auto)     false
              -- Autothrottle input must be ui
              store (cl_req ~> CL.set_autothr_src_ui)  true
              store (cl_req ~> CL.set_autothr_src_nav) false
              -- Stab input must be ui:
              store (cl_req ~> CL.set_stab_src_ui)  true
              store (cl_req ~> CL.set_stab_src_nav) false
              -- Heading input irrevelent
              store (cl_req ~> CL.set_head_src_ui)  false
              store (cl_req ~> CL.set_head_src_nav) false
          ]

  taskModuleDef $ do
    incl init_proc
    incl new_sample_proc
    incl no_sample_proc
    incl get_cl_req_proc

  return ModeSwitch
    { ms_init       = call_ init_proc
    , ms_new_sample = call_ new_sample_proc
    , ms_no_sample  = call_ no_sample_proc
    , ms_get_cl_req = call_ get_cl_req_proc
    }
  where

  modeswitchpos_from_ppm :: I.PPM -> ThreePositionSwitch
  modeswitchpos_from_ppm ppm = foldr (match_mode_map ppm) posDown mode_ppm_map
  match_mode_map ppm (mode, (min_ppm, max_ppm)) def =
    ((ppm >=? min_ppm) .&& (ppm <=? max_ppm)) ? (mode, def)

  mode_ppm_map :: [(ThreePositionSwitch, (I.PPM, I.PPM))]
  mode_ppm_map =  [(posUp,    (minBound, 1300))
                  ,(posCenter,(1301, 1700))
                  ,(posDown,  (1701, maxBound))
                  ]

