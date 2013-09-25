{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.RadioInfo where

import Ivory.Language

radioInfoTypeModule :: Module
radioInfoTypeModule = package "radio_info_type" $ do
  defStruct (Proxy :: Proxy "radio_info")

-- XXX this is total rubbish:
[ivory|
struct radio_info
  { lat     :: Stored Sint32
  ; lon     :: Stored Sint32
  ; gps_alt :: Stored Sint32
  ; vx      :: Stored Sint16
  ; vy      :: Stored Sint16
  ; vz      :: Stored Sint16
  ; time    :: Stored Uint32
  }
|]


