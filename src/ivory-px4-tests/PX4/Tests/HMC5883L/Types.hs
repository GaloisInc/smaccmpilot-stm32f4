{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PX4.Tests.HMC5883L.Types where

import Ivory.Language

hmc5883lTypesModule :: Module
hmc5883lTypesModule = package "hmc5883l_types" $ do
  defStruct (Proxy :: Proxy "hmc5883l_sample")

[ivory|
struct hmc5883l_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored Uint16)
  }
|]

