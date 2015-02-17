{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.PPM.PulseCapture.Types where

import Ivory.Language

[ivory|
struct pulse_capture
  { width :: Stored Uint16
  ; missed :: Stored IBool
  }
|]

pulseCaptureTypeModule :: Module
pulseCaptureTypeModule = package "pulse_capture_type" $ do
  defStruct (Proxy :: Proxy "pulse_capture")

