{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.MotorsOutput where

import Ivory.Language

motorsOutputTypeModule :: Module
motorsOutputTypeModule = package "motorsoutput_type" $ do
  defStruct (Proxy :: Proxy "motorsoutput_result")

[ivory|
struct motorsoutput_result
  { armed    :: Stored IBool
  ; throttle :: Stored IFloat 
  ; roll     :: Stored IFloat 
  ; pitch    :: Stored IFloat 
  ; yaw      :: Stored IFloat 
  ; time     :: Stored Uint32
  }
|]


