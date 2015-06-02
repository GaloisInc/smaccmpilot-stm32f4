{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.INS.Ivory (
  kalmanInit, kalmanPredict,
  magMeasure, accelMeasure,

  module SMACCMPilot.INS.Types
) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Ivory.Language
import Ivory.Stdlib
import Linear
import Numeric.AD
import Numeric.Estimator
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.Symbolic
import Prelude hiding (mapM, sequence_)
import SMACCMPilot.INS.Parameters
import SMACCMPilot.INS.SensorFusion
import SMACCMPilot.INS.Simulate
import SMACCMPilot.INS.Types

instance HasAtan2 IFloat where
  arctan2 = atan2F

vec3FromArray :: IvoryArea v => V3 ((Ref s t -> Ref s (Array 3 v)) -> Ref s t -> Ref s v)
vec3FromArray = V3 ((! 0) .) ((! 1) .) ((! 2) .)

stateVectorFromStruct :: Ref s (Struct "kalman_state") -> StateVector (Ref s (Stored IFloat))
stateVectorFromStruct s = StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> orient)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> gyro_bias)
  , stateMagNED = NED vec3FromArray <*> pure (~> mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> mag_xyz)
  } <*> pure s

covarianceFromStruct :: Ref s (Struct "kalman_covariance") -> StateVector (StateVector (Ref s (Stored IFloat)))
covarianceFromStruct s = stateVectorFromStruct <$> (StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> cov_orient)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> cov_gyro_bias)
  , stateMagNED = NED vec3FromArray <*> pure (~> cov_mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> cov_mag_xyz)
  } <*> pure s)

storeRow :: (Applicative f, Foldable f, IvoryStore a, IvoryExpr a) => f (Ref s (Stored a)) -> f a -> Ivory eff ()
storeRow vars vals = sequence_ $ liftA2 store vars vals

kalmanInit :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> XYZ IFloat -> XYZ IFloat -> Ivory eff ()
kalmanInit state_ptr cov_ptr acc mag = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  let initialState = initDynamic acc mag (pure 0) 0
  storeRow stateVector initialState
  sequence_ $ liftA2 storeRow covariance initCovariance

kalmanPredict :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> DisturbanceVector IFloat -> Ivory eff ()
kalmanPredict state_ptr cov_ptr dt distVector = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  stateVectorTemp <- mapM deref stateVector
  pTemp <- mapM (mapM deref) covariance
  let noise = processNoise dt
  let KalmanFilter stateVector' covariance' = augmentProcess (EKFProcess $ processModel $ auto dt) distVector (scaled noise) (scaled distCovariance) $ KalmanFilter stateVectorTemp pTemp
  storeRow stateVector $ fixQuat stateVector'
  sequence_ $ liftA2 storeRow covariance covariance'

applyUpdate :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> (KalmanFilter StateVector IFloat -> (IFloat, IFloat, KalmanFilter StateVector IFloat)) -> Ivory eff ()
applyUpdate state_ptr cov_ptr cov fusionStep = do
    let stateVector = stateVectorFromStruct state_ptr
    let covariance = covarianceFromStruct cov_ptr
    stateVectorTemp <- mapM deref stateVector
    pTemp <- mapM (mapM deref) covariance
    let (innov, innovCov, KalmanFilter stateVector' covariance') = fusionStep $ KalmanFilter stateVectorTemp pTemp
    let forceUpdate = true -- filter state is not yet right; muddle through anyway
    -- TODO: when innovCov < cov, add cov to the "right" elements of covariance
    when (forceUpdate .|| innovCov >=? cov) $ do
      when (forceUpdate .|| innov ^ (2 :: Int) / innovCov <? 5 ^ (2 :: Int)) $ do
        storeRow stateVector $ fixQuat stateVector'
        sequence_ $ liftA2 storeRow covariance covariance'

magMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> XYZ IFloat -> Ivory eff ()
magMeasure state_ptr cov_ptr mag = sequence_ $ applyUpdate state_ptr cov_ptr <$> magNoise <*> (fuseMag <*> magNoise <*> mag)

accelMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> XYZ IFloat -> Ivory eff ()
accelMeasure state_ptr cov_ptr accel = sequence_ $ applyUpdate state_ptr cov_ptr <$> accelNoise <*> (fuseAccel <*> accelNoise <*> accel)
