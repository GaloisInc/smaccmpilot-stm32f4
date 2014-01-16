{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.GPS.Types.Position where

import Ivory.Language

import SMACCMPilot.Hardware.GPS.Types.GPSFix

-- fix: see RegTypes
-- num_sv: number of space vehicles, integer
-- dop: dillution of precision estimate, meters
-- lat, lon: degrees *1E7
-- alt: meters * 1000 (mm)
-- velocities: m/s*100 (cm/s)
-- heading: degrees

[ivory|
struct position
  { fix     :: Stored GPSFix
  ; num_sv  :: Stored Uint8
  ; dop     :: Stored IFloat

  ; lat     :: Stored Sint32
  ; lon     :: Stored Sint32
  ; alt     :: Stored Sint32

  ; vnorth  :: Stored Sint32
  ; veast   :: Stored Sint32
  ; vdown   :: Stored Sint32
  ; vground :: Stored Uint32
  ; heading :: Stored IFloat

  ; time    :: Stored Uint32
  }
|]

