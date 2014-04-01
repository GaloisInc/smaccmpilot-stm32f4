{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.ControlLaw where

import Ivory.Language
import Ivory.Tower.Types.Time

import SMACCMPilot.Flight.Types.ArmedMode
import SMACCMPilot.Flight.Types.ControlSource
import SMACCMPilot.Flight.Types.ThrottleMode
import SMACCMPilot.Flight.Types.UISource
import SMACCMPilot.Flight.Types.YawMode

[ivory|
struct control_law
  { armed_mode     :: Stored ArmedMode
  ; ui_source      :: Stored UISource
  ; yaw_mode       :: Stored YawMode
  ; thr_mode       :: Stored ThrottleMode
  ; autothr_source :: Stored ControlSource
  ; stab_source    :: Stored ControlSource
  ; head_source    :: Stored ControlSource
  ; time           :: Stored ITime
  }
|]

controlLawTypeModule :: Module
controlLawTypeModule = package "control_law_type" $ do
  defStruct (Proxy :: Proxy "control_law")
