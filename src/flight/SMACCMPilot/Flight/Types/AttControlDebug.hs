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
  { head_setpt      :: Stored IFloat
  ; head_rate_setpt :: Stored IFloat
  }
|]

