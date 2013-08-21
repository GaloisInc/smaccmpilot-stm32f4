{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.Motors where

import Ivory.Language

motorsTypeModule :: Module
motorsTypeModule = package "motors_type" $ do
  defStruct (Proxy :: Proxy "motors")

-- Motor values are PWM values, measured in milliseconds.
-- 1000 is the minimum output, 2000 is the maximum output.
-- All values below 1000 will be considered 1000, all values above 2000 will be
-- considered 2000.
[ivory|
struct motors
  { motor1  :: Stored Uint16
  ; motor2  :: Stored Uint16
  ; motor3  :: Stored Uint16
  ; motor4  :: Stored Uint16
  ; time    :: Stored Uint32
  }
|]

