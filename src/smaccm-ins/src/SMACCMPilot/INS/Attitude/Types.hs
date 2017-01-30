{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.INS.Attitude.Types where

import Ivory.Language
import Ivory.Serialize

[ivory|
struct att_kalman_state
  { orient :: Array 4 (Stored IFloat)
  ; mag_ned :: Array 3 (Stored IFloat)
  }

struct att_kalman_covariance
  { cov_orient :: Array 4 (Struct att_kalman_state)
  ; cov_mag_ned :: Array 3 (Struct att_kalman_state)
  }
|]

insTypesModule :: Module
insTypesModule = package "ins_types" $ do
  defStruct (Proxy :: Proxy "att_kalman_state")
  defStruct (Proxy :: Proxy "att_kalman_covariance")
  depend serializeModule
  wrappedPackMod kalmanStateWrapper

kalmanStateWrapper :: WrappedPackRep ('Struct "att_kalman_state")
kalmanStateWrapper = wrapPackRep "att_kalman_state" $ packStruct
  [ packLabel orient
  , packLabel mag_ned
  ]

instance Packable ('Struct "att_kalman_state") where
  packRep = wrappedPackRep kalmanStateWrapper
