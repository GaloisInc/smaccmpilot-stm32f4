{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.Servos where

import Ivory.Language

servosTypeModule :: Module
servosTypeModule = package "servos_type" $ do
  defStruct (Proxy :: Proxy "servos")

[ivory|
struct servos
  { valid   :: Stored IBool
  ; servo1  :: Stored Uint16
  ; servo2  :: Stored Uint16
  ; servo3  :: Stored Uint16
  ; servo4  :: Stored Uint16
  ; time    :: Stored Uint32
  }
|]

