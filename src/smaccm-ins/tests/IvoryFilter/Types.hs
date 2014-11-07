{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module IvoryFilter.Types where

import Ivory.Language

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
