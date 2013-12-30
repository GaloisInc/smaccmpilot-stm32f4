{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.PosControlDebug where

import Ivory.Language

posControlDebugTypeModule :: Module
posControlDebugTypeModule = package "pos_control_dbg_type" $ do
  defStruct (Proxy :: Proxy "pos_control_dbg")

[ivory|
struct pos_control_dbg
  { x_vel_setpt :: Stored IFloat
  ; y_vel_setpt :: Stored IFloat
  ; head_setpt  :: Stored IFloat
  ; lat_setpt   :: Stored Sint32
  ; lon_setpt   :: Stored Sint32
  ; x_deviation :: Stored IFloat
  ; y_deviation :: Stored IFloat
  }
|]

