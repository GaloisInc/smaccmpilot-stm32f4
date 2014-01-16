{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.NavCommand where

import Ivory.Language
import SMACCMPilot.Flight.Types.EnableDisable

navCommandTypeModule :: Module
navCommandTypeModule = package "nav_command_type" $ do
  defStruct (Proxy :: Proxy "nav_command")

[ivory|
struct nav_command
  { velocity_control  :: Stored EnableDisable
  ; vel_x_setpt       :: Stored IFloat
  ; vel_y_setpt       :: Stored IFloat
  ; position_control  :: Stored EnableDisable
  ; lat_setpt         :: Stored Sint32
  ; lon_setpt         :: Stored Sint32
  ; altitude_control  :: Stored EnableDisable
  ; alt_setpt         :: Stored IFloat
  ; alt_rate_setpt    :: Stored IFloat
  ; heading_control   :: Stored EnableDisable
  ; heading_setpt     :: Stored IFloat
  ; autoland_active   :: Stored EnableDisable
  ; autoland_complete :: Stored EnableDisable
  ; time              :: Stored Uint32
  }
|]

