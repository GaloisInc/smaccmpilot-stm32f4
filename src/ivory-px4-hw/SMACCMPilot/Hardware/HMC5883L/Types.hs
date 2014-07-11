{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SMACCMPilot.Hardware.HMC5883L.Types where

import Ivory.Language
import Ivory.Tower.Types.Time

hmc5883lTypesModule :: Module
hmc5883lTypesModule = package "hmc5883l_types" $ do
  defStruct (Proxy :: Proxy "hmc5883l_sample")

[ivory|
struct hmc5883l_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- Gauss
  ; time       :: Stored ITime
  }
|]

