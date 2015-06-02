{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.INS.Types where

import Ivory.Language
import Ivory.Serialize

[ivory|
struct kalman_state
  { orient :: Array 4 (Stored IFloat)
  ; gyro_bias :: Array 3 (Stored IFloat)
  ; mag_ned :: Array 3 (Stored IFloat)
  ; mag_xyz :: Array 3 (Stored IFloat)
  }

struct kalman_covariance
  { cov_orient :: Array 4 (Struct kalman_state)
  ; cov_gyro_bias :: Array 3 (Struct kalman_state)
  ; cov_mag_ned :: Array 3 (Struct kalman_state)
  ; cov_mag_xyz :: Array 3 (Struct kalman_state)
  }
|]

insTypesModule :: Module
insTypesModule = package "ins_types" $ do
  defStruct (Proxy :: Proxy "kalman_state")
  defStruct (Proxy :: Proxy "kalman_covariance")
  depend serializeModule
  wrappedPackMod kalmanStateWrapper

kalmanStateWrapper :: WrappedPackRep (Struct "kalman_state")
kalmanStateWrapper = wrapPackRep "kalman_state" $ packStruct
  [ packLabel orient
  , packLabel gyro_bias
  , packLabel mag_ned
  , packLabel mag_xyz
  ]

instance Packable (Struct "kalman_state") where
  packRep = wrappedPackRep kalmanStateWrapper
