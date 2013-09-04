{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.Motors where

import Ivory.Language

motorsTypeModule :: Module
motorsTypeModule = package "motors_type" $ do
  defStruct (Proxy :: Proxy "motors")

-- Motor values are on a scale of 0 to 1
[ivory|
struct motors
  { frontleft  :: Stored IFloat
  ; frontright :: Stored IFloat
  ; backleft   :: Stored IFloat
  ; backright  :: Stored IFloat
  }
|]

