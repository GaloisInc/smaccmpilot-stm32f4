{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.ControlSetpoint where

import Ivory.Language
import Ivory.Tower.Types.Time

controlSetpointTypeModule :: Module
controlSetpointTypeModule = package "control_setpoint_type" $ do
  defStruct (Proxy :: Proxy "control_setpoint")

[ivory|
struct control_setpoint
  { altitude :: Stored IFloat
  ; alt_rate :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; heading  :: Stored IFloat
  ; time     :: Stored ITime
  }

|]

