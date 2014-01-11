{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.NavLaw where

import Ivory.Language

navLawTypeModule :: Module
navLawTypeModule = package "nav_law_type" $ do
  defStruct (Proxy :: Proxy "nav_law")

[ivory|
struct nav_law
  { velocity_control  :: Stored IBool
  ; vel_x_setpt       :: Stored IFloat
  ; vel_y_setpt       :: Stored IFloat
  ; position_control  :: Stored IBool
  ; lat_setpt         :: Stored Sint32
  ; lon_setpt         :: Stored Sint32
  ; altitude_control  :: Stored IBool
  ; alt_setpt         :: Stored IFloat
  ; alt_rate_setpt    :: Stored IFloat
  ; heading_control   :: Stored IBool
  ; heading_setpt     :: Stored IFloat
  ; autoland_active   :: Stored IBool
  ; autoland_complete :: Stored IBool
  ; time              :: Stored Uint32
  }
|]

