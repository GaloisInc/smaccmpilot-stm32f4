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
  ; vel :: Array 3 (Stored IFloat)
  ; pos :: Array 3 (Stored IFloat)
  ; gyro_bias :: Array 3 (Stored IFloat)
  ; wind :: Array 3 (Stored IFloat)
  ; mag_ned :: Array 3 (Stored IFloat)
  ; mag_xyz :: Array 3 (Stored IFloat)
  }

struct kalman_covariance
  { cov_orient :: Array 4 (Struct kalman_state)
  ; cov_vel :: Array 3 (Struct kalman_state)
  ; cov_pos :: Array 3 (Struct kalman_state)
  ; cov_gyro_bias :: Array 3 (Struct kalman_state)
  ; cov_wind :: Array 3 (Struct kalman_state)
  ; cov_mag_ned :: Array 3 (Struct kalman_state)
  ; cov_mag_xyz :: Array 3 (Struct kalman_state)
  }
|]

insTypesModule :: Module
insTypesModule = package "ins_types" $ do
  defStruct (Proxy :: Proxy "kalman_state")
  defStruct (Proxy :: Proxy "kalman_covariance")
  depend serializeModule
  incl statePackRef
  incl stateUnpackRef

statePackRef :: Def ('[ Ref s1 (CArray (Stored Uint8))
                      , Uint32
                      , ConstRef s2 (Struct "kalman_state")
                      ] :-> ())
statePackRef = proc "state_pack_ref" $ \ buf off msg -> body $ do
  packRef buf (off + 0 * 4) (msg ~> orient)
  packRef buf (off + 4 * 4) (msg ~> vel)
  packRef buf (off + 7 * 4) (msg ~> pos)
  packRef buf (off + 10 * 4) (msg ~> gyro_bias)
  packRef buf (off + 13 * 4) (msg ~> wind)
  packRef buf (off + 16 * 4) (msg ~> mag_ned)
  packRef buf (off + 19 * 4) (msg ~> mag_xyz)

stateUnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                        , Uint32
                        , Ref s2 (Struct "kalman_state")
                        ] :-> ())
stateUnpackRef = proc "state_unpack_ref" $ \ buf off msg -> body $ do
  unpackRef buf (off + 0 * 4) (msg ~> orient)
  unpackRef buf (off + 4 * 4) (msg ~> vel)
  unpackRef buf (off + 7 * 4) (msg ~> pos)
  unpackRef buf (off + 10 * 4) (msg ~> gyro_bias)
  unpackRef buf (off + 13 * 4) (msg ~> wind)
  unpackRef buf (off + 16 * 4) (msg ~> mag_ned)
  unpackRef buf (off + 19 * 4) (msg ~> mag_xyz)

instance SerializableRef (Struct "kalman_state") where
  packRef = call_ statePackRef
  unpackRef = call_ stateUnpackRef
