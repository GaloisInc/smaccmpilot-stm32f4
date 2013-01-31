{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module PositionEstimateType where

import Ivory.Language

positionEstimateTypeModule :: Module
positionEstimateTypeModule = package "position_estimate_type" $ do
  defStruct (Proxy :: Proxy "position_estimate")

[ivory|
struct position_estimate
  { horiz_conf :: Stored Uint8
  ; vx         :: Stored IFloat
  ; vy         :: Stored IFloat
  ; vert_conf  :: Stored Uint8
  ; alt        :: Stored IFloat
  ; vz         :: Stored IFloat
  ; time       :: Stored Uint32
  }
|]


