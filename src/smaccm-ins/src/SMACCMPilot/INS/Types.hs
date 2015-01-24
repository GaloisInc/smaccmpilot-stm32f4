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
  { orient :: Array 4 (Stored IDouble)
  ; vel :: Array 3 (Stored IDouble)
  ; pos :: Array 3 (Stored IDouble)
  ; gyro_bias :: Array 3 (Stored IDouble)
  ; wind :: Array 3 (Stored IDouble)
  ; mag_ned :: Array 3 (Stored IDouble)
  ; mag_xyz :: Array 3 (Stored IDouble)
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
  packRef buf (off + 0 * 8) (msg ~> orient)
  packRef buf (off + 4 * 8) (msg ~> vel)
  packRef buf (off + 7 * 8) (msg ~> pos)
  packRef buf (off + 10 * 8) (msg ~> gyro_bias)
  packRef buf (off + 13 * 8) (msg ~> wind)
  packRef buf (off + 16 * 8) (msg ~> mag_ned)
  packRef buf (off + 19 * 8) (msg ~> mag_xyz)

stateUnpackRef :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                        , Uint32
                        , Ref s2 (Struct "kalman_state")
                        ] :-> ())
stateUnpackRef = proc "state_unpack_ref" $ \ buf off msg -> body $ do
  unpackRef buf (off + 0 * 8) (msg ~> orient)
  unpackRef buf (off + 4 * 8) (msg ~> vel)
  unpackRef buf (off + 7 * 8) (msg ~> pos)
  unpackRef buf (off + 10 * 8) (msg ~> gyro_bias)
  unpackRef buf (off + 13 * 8) (msg ~> wind)
  unpackRef buf (off + 16 * 8) (msg ~> mag_ned)
  unpackRef buf (off + 19 * 8) (msg ~> mag_xyz)

instance SerializableRef (Struct "kalman_state") where
  packRef = call_ statePackRef
  unpackRef = call_ stateUnpackRef
