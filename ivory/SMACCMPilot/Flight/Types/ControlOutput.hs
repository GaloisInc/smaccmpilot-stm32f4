{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.ControlOutput where

import Ivory.Language

controlOutputTypeModule :: Module
controlOutputTypeModule = package "controloutput_type" $ do
  defStruct (Proxy :: Proxy "controloutput_result")

[ivory|
struct controloutput_result
  { armed    :: Stored IBool
  ; throttle :: Stored IFloat 
  ; roll     :: Stored IFloat 
  ; pitch    :: Stored IFloat 
  ; yaw      :: Stored IFloat 
  ; time     :: Stored Uint32
  }
|]


