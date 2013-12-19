{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.ControlLawRequest where

import Ivory.Language

[ivory|
struct control_law_request
  { set_safe             :: Stored IBool
  ; set_disarmed         :: Stored IBool
  ; set_armed            :: Stored IBool
  ; set_stab_ppm         :: Stored IBool
  ; set_stab_mavlink     :: Stored IBool
  ; set_stab_auto        :: Stored IBool
  ; set_yaw_rate         :: Stored IBool
  ; set_yaw_heading      :: Stored IBool
  ; set_thr_direct       :: Stored IBool
  ; set_thr_auto         :: Stored IBool
  ; set_autothr_ppm      :: Stored IBool
  ; set_autothr_mavlink  :: Stored IBool
  ; set_autothr_auto     :: Stored IBool
  ; time                 :: Stored Uint32
  }
|]

controlLawRequestTypeModule :: Module
controlLawRequestTypeModule = package "control_law_request_type" $ do
  defStruct (Proxy :: Proxy "control_law_request")

initControlLawRequest :: [InitStruct "control_law_request"]
                      -> Init (Struct "control_law_request")
initControlLawRequest is = istruct (defaults ++ is)
  where
  defaults =
    [ set_safe             .= ival false
    , set_disarmed         .= ival false
    , set_armed            .= ival false
    , set_stab_ppm         .= ival false
    , set_stab_mavlink     .= ival false
    , set_stab_auto        .= ival false
    , set_yaw_rate         .= ival false
    , set_yaw_heading      .= ival false
    , set_thr_direct       .= ival false
    , set_thr_auto         .= ival false
    , set_autothr_ppm      .= ival false
    , set_autothr_mavlink  .= ival false
    , set_autothr_auto     .= ival false
    , time                 .= ival 0
    ]

