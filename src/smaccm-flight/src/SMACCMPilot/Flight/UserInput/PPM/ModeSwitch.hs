{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Flight.UserInput.PPM.ModeSwitch
  ( ModeSwitch(..)
  , monitorModeSwitch
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib

import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw as CL
import qualified SMACCMPilot.Comm.Ivory.Types.ControlSource as CS
import qualified SMACCMPilot.Comm.Ivory.Types.YawMode as Y
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleMode as T

data ModeSwitch =
  ModeSwitch
    { ms_init       :: forall eff   . Ivory eff ()
    , ms_new_sample :: forall eff s . Ref s (Array 8 (Stored Uint16))
                                   -> ITime -> Ivory eff ()
    , ms_no_sample  :: forall eff   . Ivory eff ()
    , ms_get_cl_req :: forall eff s . Ref s (Struct "control_law")
                                   -> Ivory eff ()
    }

newtype ThreePositionSwitch = ThreePositionSwitch Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

posUp :: ThreePositionSwitch
posUp = ThreePositionSwitch 0
posCenter :: ThreePositionSwitch
posCenter = ThreePositionSwitch 1
posDown :: ThreePositionSwitch
posDown = ThreePositionSwitch 2

monitorModeSwitch :: Monitor p ModeSwitch
monitorModeSwitch = do
  md_last_position      <- state "md_last_position"
  md_last_position_time <- state "md_last_position_time"
  let named n = fmap showUnique $ freshname $ "ppmdecoder_modeswitch_" ++ n

  init_name <- named "init"
  new_sample_name <- named "new_sample"
  no_sample_name <- named "no_sample"
  get_cl_req_name <- named "cl_req_proc"

  let init_proc :: Def('[]:->())
      init_proc = proc init_name $ body $ do
        store md_last_position posDown
        store md_last_position_time 0

      new_sample_proc :: Def('[Ref s (Array 8 (Stored Uint16)), ITime]:->())
      new_sample_proc = proc new_sample_name $ \ppms time -> body $ do
        switch <- deref (ppms ! (4 :: Ix 8))
        position <- assign $ modeswitchpos_from_ppm switch
        -- XXX debouncing
        store md_last_position position
        store md_last_position_time time

      -- No failure logic--handled in arming state machine.
      no_sample_proc :: Def('[] :-> ())
      no_sample_proc = proc no_sample_name $ body $ return ()

      get_cl_req_proc :: Def('[Ref s (Struct "control_law")]:->())
      get_cl_req_proc = proc get_cl_req_name $ \cl -> body $ do
        p <- deref md_last_position
        cond_
          [ p ==? posUp ==> do
              -- UI source may be gcs
              store (cl ~> CL.ui_mode) CS.gcs
              -- Yaw law must be heading
              store (cl ~> CL.yaw_mode) Y.heading
              -- Throttle law must be autothrottle
              store (cl ~> CL.thr_mode) T.auto
          , p ==? posCenter ==> do
              -- UI source must be ppm
              store (cl ~> CL.ui_mode) CS.ppm
              -- Yaw law must be rate
              store (cl ~> CL.yaw_mode) Y.rate
              -- Throttle law must be autothrottle
              store (cl ~> CL.thr_mode) T.auto
          , p ==? posDown ==> do
              -- UI source must be ppm
              store (cl ~> CL.ui_mode) CS.ppm
              -- Yaw law must be rate
              store (cl ~> CL.yaw_mode) Y.rate
              -- Throttle law must be direct
              store (cl ~> CL.thr_mode) T.direct
          ]

  monitorModuleDef $ do
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

  modeswitchpos_from_ppm :: Uint16 -> ThreePositionSwitch
  modeswitchpos_from_ppm ppm = foldr (match_mode_map ppm) posDown mode_ppm_map
  match_mode_map ppm (mode, (min_ppm, max_ppm)) def =
    ((ppm >=? min_ppm) .&& (ppm <=? max_ppm)) ? (mode, def)

  mode_ppm_map :: [(ThreePositionSwitch, (Uint16, Uint16))]
  mode_ppm_map =  [(posUp,    (minBound, 1300))
                  ,(posCenter,(1301, 1700))
                  ,(posDown,  (1701, maxBound))
                  ]

