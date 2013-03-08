{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.UserInput where

import Ivory.Language

userInputTypeModule :: Module
userInputTypeModule = package "userinput_type" $ do
  defStruct (Proxy :: Proxy "userinput_result")

[ivory|
struct userinput_result
  { armed    :: Stored IBool
  ; throttle :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; yaw      :: Stored IFloat
  ; mode     :: Stored Uint8
  ; time     :: Stored Uint32
  }
|]

