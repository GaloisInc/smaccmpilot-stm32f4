{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Flight.Types.MaybeFloat where

import Ivory.Language
import Ivory.Stdlib

[ivory|
struct maybe_float
  { mf_valid              :: Stored IBool
  ; mf_value              :: Stored IFloat
  }
|]

instance MaybeType "maybe_float" IFloat where
  maybeValidLabel = mf_valid
  maybeValueLabel = mf_value

maybeFloatModule :: Module
maybeFloatModule = package "maybe_float_type" $ do
  defStruct (Proxy :: Proxy "maybe_float")
