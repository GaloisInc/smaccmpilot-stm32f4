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
  ; servo   :: Array 4 (Stored Uint16)
  ; time    :: Stored Uint32
  }
|]


