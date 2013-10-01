{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.RadioStat where

import Ivory.Language

radioStatTypeModule :: Module
radioStatTypeModule = package "radio_stat_type" $ do
  defStruct (Proxy :: Proxy "radio_stat")

[ivory|
struct radio_stat
  { sik       :: Stored IBool
  ; loc_rssi  :: Stored Uint8
  ; loc_noise :: Stored Uint8
  ; loc_rxctr :: Stored Uint16
  ; rem_rssi  :: Stored Uint8
  ; rem_noise :: Stored Uint8
  ; rem_rxctr :: Stored Uint16
  ; tx_err    :: Stored Uint16
  ; rx_err    :: Stored Uint16
  ; tx_ovf    :: Stored Uint16
  ; rx_ovf    :: Stored Uint16
  ; ecc_errs  :: Stored Uint16
  ; ecc_pkts  :: Stored Uint16
  }
|]


