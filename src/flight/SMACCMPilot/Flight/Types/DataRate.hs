{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.Types.DataRate where

import Ivory.Language

dataRateTypeModule :: Module
dataRateTypeModule = package "data_rate" $ do
  defStruct (Proxy :: Proxy "data_rate_state")

[ivory|

struct data_rate_state
  { -- How many messages have we dropped?
    dropped  :: Stored Uint32
    -- When was the last successful message sent?
  ; lastSucc :: Stored Uint32
  }

|]
