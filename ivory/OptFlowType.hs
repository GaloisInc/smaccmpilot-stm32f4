{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module OptFlowType where

import Ivory.Language

optFlowTypeModule :: Module
optFlowTypeModule = package "optflow_type" $ do
  defStruct (Proxy :: Proxy "optflow_result")

[ivory|
struct optflow_result
  { valid       :: Stored IBool
  ; ground_dist :: Stored IFloat
  ; flow_x      :: Stored IFloat
  ; flow_y      :: Stored IFloat
  ; quality     :: Stored Uint8
  ; sensortime  :: Stored Uint64
  ; time        :: Stored Uint32
  }
|]


