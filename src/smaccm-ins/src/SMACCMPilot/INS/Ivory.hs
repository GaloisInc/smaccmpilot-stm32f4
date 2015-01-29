{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.INS.Ivory (
  kalmanInit, kalmanPredict,
  velMeasure, posMeasure, pressureMeasure, tasMeasure, magMeasure,

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
import Numeric.Estimator.Model.Pressure
import Numeric.Estimator.Model.SensorFusion
import Numeric.Estimator.Model.Symbolic
import Prelude hiding (mapM, sequence_)
import SMACCMPilot.INS.Parameters
import SMACCMPilot.INS.Simulate
import SMACCMPilot.INS.Types

instance HasAtan2 IFloat where
  arctan2 = atan2F

vec3FromArray :: IvoryArea v => V3 ((Ref s t -> Ref s (Array 3 v)) -> Ref s t -> Ref s v)
vec3FromArray = V3 ((! 0) .) ((! 1) .) ((! 2) .)

stateVectorFromStruct :: Ref s (Struct "kalman_state") -> StateVector (Ref s (Stored IFloat))
stateVectorFromStruct s = StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> orient)
  , stateVel = NED vec3FromArray <*> pure (~> vel)
  , statePos = NED vec3FromArray <*> pure (~> pos)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> gyro_bias)
  , stateWind = NED vec3FromArray <*> pure (~> wind)
  , stateMagNED = NED vec3FromArray <*> pure (~> mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> mag_xyz)
  } <*> pure s

covarianceFromStruct :: Ref s (Struct "kalman_covariance") -> StateVector (StateVector (Ref s (Stored IFloat)))
covarianceFromStruct s = stateVectorFromStruct <$> (StateVector
  { stateOrient = Quaternion ((! 0) .) (V3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> cov_orient)
  , stateVel = NED vec3FromArray <*> pure (~> cov_vel)
  , statePos = NED vec3FromArray <*> pure (~> cov_pos)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> cov_gyro_bias)
  , stateWind = NED vec3FromArray <*> pure (~> cov_wind)
  , stateMagNED = NED vec3FromArray <*> pure (~> cov_mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> cov_mag_xyz)
  } <*> pure s)

storeRow :: (Applicative f, Foldable f, IvoryStore a, IvoryExpr a) => f (Ref s (Stored a)) -> f a -> Ivory eff ()
storeRow vars vals = sequence_ $ liftA2 store vars vals

kalmanInit :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> XYZ IFloat -> XYZ IFloat -> IFloat -> Ivory eff ()
kalmanInit state_ptr cov_ptr acc mag pressure = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  let depth = negate $ pressureToHeight pressure
  let initialState = initDynamic acc mag (pure 0) 0 (pure 0) (ned 0 0 depth)
  storeRow stateVector initialState
  sequence_ $ liftA2 storeRow covariance initCovariance

kalmanPredict :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> DisturbanceVector IFloat -> Ivory eff ()
kalmanPredict state_ptr cov_ptr dt distVector = do
  let stateVector = stateVectorFromStruct state_ptr
  let covariance = covarianceFromStruct cov_ptr
  stateVectorTemp <- mapM deref stateVector
  pTemp <- mapM (mapM deref) covariance
  let speed = norm $ stateVel stateVectorTemp
  onGround <- assign $ speed <? 4
  let whenFlying v = onGround ? (v, 0)
  let noise = (pure id) { stateWind = pure whenFlying, stateMagNED = pure whenFlying, stateMagXYZ = pure whenFlying } <*> processNoise dt
  let KalmanFilter stateVector' covariance' = augmentProcess (EKFProcess $ processModel $ auto dt) distVector (scaled noise) (scaled distCovariance) $ KalmanFilter stateVectorTemp pTemp
  storeRow stateVector $ fixQuat stateVector'
  sequence_ $ liftA2 storeRow covariance covariance'

applyUpdate :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> (KalmanFilter StateVector IFloat -> (IFloat, IFloat, KalmanFilter StateVector IFloat)) -> Ivory eff ()
applyUpdate state_ptr cov_ptr cov fusionStep = do
    let stateVector = stateVectorFromStruct state_ptr
    let covariance = covarianceFromStruct cov_ptr
    stateVectorTemp <- mapM deref stateVector
    pTemp <- mapM (mapM deref) covariance
    let (innov, innovCov, KalmanFilter stateVectorPreCorrected covariance') = fusionStep $ KalmanFilter stateVectorTemp pTemp
    let stateVector' = fixQuat stateVectorPreCorrected
    let forceUpdate = true -- filter state is not yet right; muddle through anyway
    -- TODO: when innovCov < cov, add cov to the "right" elements of covariance
    when (forceUpdate .|| innovCov >=? cov) $ do
      when (forceUpdate .|| innov ^ (2 :: Int) / innovCov <? 5 ^ (2 :: Int)) $ do
        let save :: (Applicative f, Foldable f) => (forall a. StateVector a -> f a) -> Ivory eff ()
            save sel = do
              storeRow (sel stateVector) (sel stateVector')
              sequence_ $ liftA2 storeRow (sel covariance) (sel covariance')
        save stateOrient
        save stateVel
        save statePos
        save stateGyroBias
        let speed = norm $ stateVel stateVectorTemp
        when (speed >=? 4) $ do
          save stateWind
          save stateMagNED
          save stateMagXYZ

velMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> NED IFloat -> Ivory eff ()
velMeasure state_ptr cov_ptr velVec = sequence_ $ applyUpdate state_ptr cov_ptr <$> velNoise <*> (fuseVel <*> velNoise <*> velVec)

posMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> NED IFloat -> Ivory eff ()
posMeasure state_ptr cov_ptr posVec = sequence_ $ applyUpdate state_ptr cov_ptr <$> posNoise <*> (fusePos <*> posNoise <*> posVec)

pressureMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> Ivory eff ()
pressureMeasure state_ptr cov_ptr pressure = applyUpdate state_ptr cov_ptr pressureNoise $ fusePressure pressureNoise pressure

tasMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> IFloat -> Ivory eff ()
tasMeasure state_ptr cov_ptr tas = applyUpdate state_ptr cov_ptr tasNoise $ fuseTAS tasNoise tas

magMeasure :: Ref s1 (Struct "kalman_state") -> Ref s2 (Struct "kalman_covariance") -> XYZ IFloat -> Ivory eff ()
magMeasure state_ptr cov_ptr mag = sequence_ $ applyUpdate state_ptr cov_ptr <$> magNoise <*> (fuseMag <*> magNoise <*> mag)
