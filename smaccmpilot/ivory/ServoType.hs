{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module ServoType where

import Ivory.Language

servoModule :: Module
servoModule = package "servo_type" $ do
  defStruct (Proxy :: Proxy "servo_result")

[ivory|
struct servo_result
  { valid   :: Stored IBool
  ; servo1  :: Stored Uint16
  ; servo2  :: Stored Uint16
  ; servo3  :: Stored Uint16
  ; servo4  :: Stored Uint16
  ; time    :: Stored Uint32
  }
|]


