{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.ControlOutput where

import Ivory.Language
import Ivory.Tower.Types.Time

controlOutputTypeModule :: Module
controlOutputTypeModule = package "controloutput_type" $ do
  defStruct (Proxy :: Proxy "controloutput")

[ivory|
struct controloutput
  { armed    :: Stored IBool
  ; throttle :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; yaw      :: Stored IFloat
  ; time     :: Stored ITime
  }
|]


