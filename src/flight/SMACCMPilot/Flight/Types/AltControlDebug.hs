{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.AltControlDebug where

import Ivory.Language

altControlDebugTypeModule :: Module
altControlDebugTypeModule = package "alt_control_dbg_type" $ do
  defStruct (Proxy :: Proxy "alt_control_dbg")

[ivory|
struct alt_control_dbg
  { alt_est        :: Stored IFloat
  ; alt_rate_est   :: Stored IFloat
  ; thrust_p       :: Stored IFloat
  ; thrust_i       :: Stored IFloat
  ; thrust_d       :: Stored IFloat
  ; thrust_i_reset :: Stored IFloat
  ; ui_setp        :: Stored IFloat
  ; ui_rate_setp   :: Stored IFloat
  ; pos_p          :: Stored IFloat
  ; pos_i          :: Stored IFloat
  ; pos_d          :: Stored IFloat
  ; pos_setp       :: Stored IFloat
  ; pos_rate_setp  :: Stored IFloat
  }
|]

