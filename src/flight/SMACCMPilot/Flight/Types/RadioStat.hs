{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.RadioStat where

import Ivory.Language

radioStatTypeModule :: Module
radioStatTypeModule = package "radio_stat_type" $ do
  defStruct (Proxy :: Proxy "radio_stat")

-- XXX this is total rubbish:
[ivory|
struct radio_stat
  { lat     :: Stored Sint32
  ; lon     :: Stored Sint32
  ; gps_alt :: Stored Sint32
  ; vx      :: Stored Sint16
  ; vy      :: Stored Sint16
  ; vz      :: Stored Sint16
  ; time    :: Stored Uint32
  }
|]


