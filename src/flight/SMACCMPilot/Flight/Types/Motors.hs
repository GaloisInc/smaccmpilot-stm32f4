{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.Motors where

import Ivory.Language

motorsTypeModule :: Module
motorsTypeModule = package "motors_type" $ do
  defStruct (Proxy :: Proxy "motors")

[ivory|
struct motors
  { valid   :: Stored IBool
  ; motor1  :: Stored Uint16
  ; motor2  :: Stored Uint16
  ; motor3  :: Stored Uint16
  ; motor4  :: Stored Uint16
  ; time    :: Stored Uint32
  }
|]

