{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.ControlLaw where

import Ivory.Language

import SMACCMPilot.Flight.Types.ArmedMode
import SMACCMPilot.Flight.Types.ThrottleMode
import SMACCMPilot.Flight.Types.ControlSource

[ivory|
struct control_law
  { armed_mode    :: Stored ArmedMode
  ; stab_ctl      :: Stored ControlSource
  ; thr_mode      :: Stored ThrottleMode
  ; autothr_ctl   :: Stored ControlSource
  ; time          :: Stored Uint32
  }
|]

controlLawTypeModule :: Module
controlLawTypeModule = package "control_law_type" $ do
  defStruct (Proxy :: Proxy "control_law")
