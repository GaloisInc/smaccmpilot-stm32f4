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
