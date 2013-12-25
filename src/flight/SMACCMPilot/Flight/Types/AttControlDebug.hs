{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.AttControlDebug where

import Ivory.Language

attControlDebugTypeModule :: Module
attControlDebugTypeModule = package "att_control_dbg_type" $ do
  defStruct (Proxy :: Proxy "att_control_dbg")

[ivory|
struct att_control_dbg
  { head_setpt       :: Stored IFloat
  ; head_rate_setpt  :: Stored IFloat
  ; head_ctl_p       :: Stored IFloat
  ; head_ctl_d       :: Stored IFloat
  ; pitch_setpt      :: Stored IFloat
  ; pitch_rate_setpt :: Stored IFloat
  ; roll_setpt       :: Stored IFloat
  ; roll_rate_setpt  :: Stored IFloat
  }
|]

